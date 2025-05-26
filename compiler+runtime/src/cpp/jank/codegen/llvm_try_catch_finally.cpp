// START OF MODIFIED FILE: src/cpp/jank/codegen/llvm_try_catch_finally.cpp
#include <jank/codegen/llvm_try_catch_finally.hpp>
#include <jank/runtime/obj/nil.hpp>
#include <jank/runtime/obj/persistent_vector.hpp> /* For get_entry in catch if needed */
#include <jank/analyze/expr/call.hpp>
#include <jank/analyze/expr/throw.hpp>
#include <jank/profile/time.hpp>
#include <jank/error.hpp> /* For jank::jank_exception */
#include <llvm/IR/PredIteratorCache.h> // For llvm::predecessors

/* Ensure these are declared for LLVM to find their symbols */
#define JANK_EXPORT /* For now, assume JIT can find symbols in the host process */
extern "C"
{
  void *jank_get_exception_typeinfo();
  void *jank_get_obj_from_jank_exception_c_abi(jank::jank_exception *e);
  JANK_EXPORT
  void jank_throw_for_invoke(jank::runtime::object_ref o); /* Throws jank::jank_exception */
}

namespace jank::codegen
{
  namespace detail
  {

    llvm::Value *
    gen_expr_potentially_throwing(llvm_processor &proc_ctx,
                                  analyze::expression_ref const an_expr,
                                  analyze::expr::function_arity const &current_fn_arity,
                                  llvm::BasicBlock *normal_dest_if_invoke,
                                  llvm::BasicBlock *unwind_dest_if_invoke)
    {
      auto &builder = *proc_ctx.ctx->builder;
      auto &module = *proc_ctx.ctx->module;

      bool is_call_like = (an_expr->kind == analyze::expression_kind::call
                           || an_expr->kind == analyze::expression_kind::named_recursion);
      bool is_explicit_throw = (an_expr->kind == analyze::expression_kind::throw_);

      if((is_call_like || is_explicit_throw) && proc_ctx.m_is_in_try_block_for_invoke
         && unwind_dest_if_invoke && normal_dest_if_invoke)
      {
        if(is_explicit_throw)
        {
          auto throw_expr = jtl::static_ref_cast<analyze::expr::throw_>(an_expr);
          llvm::Value *thrown_val = proc_ctx.gen(throw_expr->value, current_fn_arity);

          llvm::FunctionType *throw_fn_ty
            = llvm::FunctionType::get(builder.getVoidTy(), { builder.getPtrTy() }, false);
          llvm::FunctionCallee throw_fn_callee
            = module.getOrInsertFunction("jank_throw_for_invoke", throw_fn_ty);

          if(auto *F = llvm::dyn_cast<llvm::Function>(throw_fn_callee.getCallee()))
          {
            F->removeFnAttr(llvm::Attribute::NoUnwind);
          }
          // The invoke itself does not produce a value for a throw.
          builder.CreateInvoke(
            throw_fn_callee,
            normal_dest_if_invoke, // Normal dest for a throw means it didn't throw (unreachable)
            unwind_dest_if_invoke, // Unwind dest
            { thrown_val });

          // The normal_dest_if_invoke should now be considered unreachable.
          // Ensure it's terminated if the builder was previously pointing to it.
          if(builder.GetInsertBlock() == normal_dest_if_invoke
             && normal_dest_if_invoke->getTerminator() == nullptr)
          {
            builder.SetInsertPoint(normal_dest_if_invoke); // Make sure we are in the right block
            builder.CreateUnreachable();
          }
          // The builder's insertion point should logically be after the invoke,
          // but control flow has branched. For a throw, no value is "returned" by the throw itself.
          // The caller of gen_expr_potentially_throwing must handle the fact that the current block
          // might be terminated and not expect a direct value from this path.
          // The `normal_dest_if_invoke` is where execution would continue if the invoke didn't unwind.
          // For a throw, this should be an unreachable block.
          return nullptr;
        }

        // Handle regular function calls with invoke
        auto call_expr = jtl::static_ref_cast<analyze::expr::call>(an_expr);

        llvm::Value *callee = proc_ctx.gen(call_expr->source_expr, current_fn_arity);
        llvm::SmallVector<llvm::Value *> arg_handles;
        llvm::SmallVector<llvm::Type *> arg_types;

        arg_handles.emplace_back(callee);
        arg_types.emplace_back(builder.getPtrTy());

        for(auto const &arg_expr_node : call_expr->arg_exprs)
        {
          arg_handles.emplace_back(proc_ctx.gen(arg_expr_node, current_fn_arity));
          arg_types.emplace_back(builder.getPtrTy());
        }

        jtl::immutable_string call_fn_name
          = llvm_processor::arity_to_call_fn(call_expr->arg_exprs.size());
        llvm::FunctionType *target_fn_type
          = llvm::FunctionType::get(builder.getPtrTy(), arg_types, false);
        llvm::FunctionCallee fn_callee
          = module.getOrInsertFunction(call_fn_name.c_str(), target_fn_type);

        if(auto *F = llvm::dyn_cast<llvm::Function>(fn_callee.getCallee()))
        {
          F->removeFnAttr(llvm::Attribute::NoUnwind);
        }

        llvm::InvokeInst *invoke_inst = builder.CreateInvoke(fn_callee,
                                                             normal_dest_if_invoke,
                                                             unwind_dest_if_invoke,
                                                             arg_handles);
        builder.SetInsertPoint(normal_dest_if_invoke); // Set builder to the normal destination
        return invoke_inst; // The result of the call
      }
      else
      {
        // Not a call/throw or not in a native try block, generate as a regular call/expression
        return proc_ctx.gen(an_expr, current_fn_arity);
      }
    }

    TryGenBlocks create_try_catch_finally_blocks(llvm_processor &proc_ctx,
                                                 analyze::expr::try_ref const expr,
                                                 llvm::Function *current_llvm_fn)
    {
      TryGenBlocks blocks{};
      blocks.try_body_entry_bb
        = llvm::BasicBlock::Create(*proc_ctx.ctx->llvm_ctx, "try.body.entry", current_llvm_fn);

      // try_continue_bb is the merge point after all try/catch/finally logic.
      // It's not parented immediately, as it depends on whether finally exists.
      blocks.try_continue_bb = llvm::BasicBlock::Create(*proc_ctx.ctx->llvm_ctx, "try.cont");

      bool needs_landing_pad = expr->catch_body.is_some() || expr->finally_body.is_some();
      if(needs_landing_pad)
      {
        blocks.landing_pad_bb
          = llvm::BasicBlock::Create(*proc_ctx.ctx->llvm_ctx, "landingpad", current_llvm_fn);
      }
      if(expr->catch_body.is_some())
      {
        blocks.catch_body_entry_bb
          = llvm::BasicBlock::Create(*proc_ctx.ctx->llvm_ctx, "catch.body.entry", current_llvm_fn);
      }
      if(expr->finally_body.is_some())
      {
        // This block is for normal flow *into* the finally decision logic
        blocks.finally_normal_path_entry_bb = llvm::BasicBlock::Create(*proc_ctx.ctx->llvm_ctx,
                                                                       "finally.normal.entry",
                                                                       current_llvm_fn);
        // This block contains the actual user-defined finally code
        blocks.finally_actual_code_bb = llvm::BasicBlock::Create(*proc_ctx.ctx->llvm_ctx,
                                                                 "finally.actual.code",
                                                                 current_llvm_fn);
        if(needs_landing_pad) // only need rethrow if there's a chance of catching an exception
        {
          blocks.rethrow_bb
            = llvm::BasicBlock::Create(*proc_ctx.ctx->llvm_ctx, "finally.rethrow", current_llvm_fn);
        }
      }
      return blocks;
    }

    std::pair<llvm::Value *, llvm::BasicBlock *>
    gen_try_block_body(llvm_processor &proc_ctx,
                       analyze::expr::try_ref const expr,
                       analyze::expr::function_arity const &caller_arity,
                       TryGenBlocks &blocks)
    {
      llvm::Function *current_llvm_fn = proc_ctx.ctx->builder->GetInsertBlock()->getParent();
      auto &builder = *proc_ctx.ctx->builder;

      builder.SetInsertPoint(blocks.try_body_entry_bb);

      llvm::Value *last_val_in_try = proc_ctx.gen_global(runtime::jank_nil);
      llvm::BasicBlock *current_normal_flow_bb = blocks.try_body_entry_bb;

      for(size_t i = 0; i < expr->body->values.size(); ++i)
      {
        // The builder's insert point is updated by gen_expr_potentially_throwing to its normal_dest
        // So, we must ensure we are at the correct point before generating the next expression.
        builder.SetInsertPoint(current_normal_flow_bb);
        if(builder.GetInsertBlock()->getTerminator())
        {
          break; // Current path has terminated (e.g. via an earlier throw or recur)
        }

        auto const current_expr_node = expr->body->values[i];
        bool is_last_in_try_body = (i == expr->body->values.size() - 1);

        llvm::BasicBlock *normal_dest_for_this_expr;
        if(is_last_in_try_body)
        {
          // If last, normal flow goes to finally (if exists) or to the try_continue block
          normal_dest_for_this_expr = expr->finally_body.is_some()
            ? blocks.finally_normal_path_entry_bb
            : blocks.try_continue_bb;
          if(!normal_dest_for_this_expr->getParent())
          { // Ensure it's added if it's try_continue_bb and not yet parented
            current_llvm_fn->insert(current_llvm_fn->end(), normal_dest_for_this_expr);
            // current_llvm_fn->getBasicBlockList().push_back(normal_dest_for_this_expr);
          }
        }
        else
        {
          // For intermediate expressions, create a new block for normal continuation
          normal_dest_for_this_expr = llvm::BasicBlock::Create(
            *proc_ctx.ctx->llvm_ctx,
            "try.stmt.next",
            current_llvm_fn,
            blocks.landing_pad_bb /* Insert before landingpad if exists */);
        }

        last_val_in_try = detail::gen_expr_potentially_throwing(proc_ctx,
                                                                current_expr_node,
                                                                caller_arity,
                                                                normal_dest_for_this_expr,
                                                                blocks.landing_pad_bb);

        // gen_expr_potentially_throwing sets builder to normal_dest_for_this_expr
        current_normal_flow_bb = builder.GetInsertBlock();

        // If the normal_dest_for_this_expr (now current_normal_flow_bb) was for an intermediate
        // statement and is not yet terminated by the invoke/call itself (e.g. it's an empty block
        // after a non-throwing call), explicitly branch to the *next* normal_dest_for_this_expr.
        // This only applies if it wasn't the last statement.
        if(!is_last_in_try_body && current_normal_flow_bb->getTerminator() == nullptr)
        {
          // This case is tricky. If gen_expr_potentially_throwing generated a `call` (not `invoke`),
          // current_normal_flow_bb would be the original block. We need to ensure flow.
          // However, gen_expr_potentially_throwing with invoke already sets builder to normal_dest_for_this_expr.
          // So, if current_normal_flow_bb is normal_dest_for_this_expr AND it's not terminated, it means
          // the invoke itself didn't terminate it (e.g. an empty block was created).
          // This logic is now mostly handled by `gen_expr_potentially_throwing` setting insert point.
          // The main purpose here is to ensure that if `normal_dest_for_this_expr` was created for an
          // intermediate statement, it gets a terminator if `gen_expr_potentially_throwing` didn't add one.
          // This typically means branching to the *next* iteration's `normal_dest_for_this_expr` implicitly
          // because `current_normal_flow_bb` will be that block in the next iteration.
        }
      }

      // After the loop, current_normal_flow_bb is the block where the last expression's normal flow ended.
      builder.SetInsertPoint(current_normal_flow_bb);
      if(builder.GetInsertBlock()->getTerminator() == nullptr)
      {
        // If the very last expression's normal flow didn't terminate, branch to finally or continue.
        llvm::BasicBlock *final_dest = expr->finally_body.is_some()
          ? blocks.finally_normal_path_entry_bb
          : blocks.try_continue_bb;
        if(!final_dest->getParent())
        {
          current_llvm_fn->insert(current_llvm_fn->end(), final_dest);
          // current_llvm_fn->getBasicBlockList().push_back(final_dest);
        }
        builder.CreateBr(final_dest);
      }

      return {
        last_val_in_try,
        current_normal_flow_bb
      }; // current_normal_flow_bb is the last active block of the try body's normal path
    }

    std::pair<llvm::Value *, llvm::BasicBlock *>
    gen_landing_pad_and_catch_block(llvm_processor &proc_ctx,
                                    analyze::expr::try_ref const expr,
                                    analyze::expr::function_arity const &caller_arity,
                                    TryGenBlocks &blocks,
                                    llvm::LandingPadInst **out_landing_pad_inst)
    {
      if(!blocks.landing_pad_bb) // No landing pad if no catch and no finally
      {
        *out_landing_pad_inst = nullptr;
        return { proc_ctx.gen_global(runtime::jank_nil), nullptr };
      }
      auto &builder = *proc_ctx.ctx->builder;
      llvm::Function *current_llvm_fn = proc_ctx.ctx->builder->GetInsertBlock()->getParent();


      builder.SetInsertPoint(blocks.landing_pad_bb);
      llvm::Type *landing_pad_type
        = llvm::StructType::get(*proc_ctx.ctx->llvm_ctx,
                                { builder.getPtrTy(), builder.getInt32Ty() });
      llvm::LandingPadInst *lp
        = builder.CreateLandingPad(landing_pad_type, 0, "lpad"); // Start with 0 clauses
      *out_landing_pad_inst = lp;

      // Clause for jank::jank_exception
      // The name _ZTIn4jank14jank_exceptionE is for Itanium ABI.
      std::string typeinfo_name = "_ZTIn4jank14jank_exceptionE";
      llvm::Type *typeinfo_struct_type
        = llvm::StructType::getTypeByName(*proc_ctx.ctx->llvm_ctx, "class.std::type_info");
      if(!typeinfo_struct_type)
      { // Ensure it's declared if not already
        typeinfo_struct_type
          = llvm::StructType::create(*proc_ctx.ctx->llvm_ctx, "class.std::type_info");
      }
      llvm::Constant *jankExceptionTypeInfoConstant
        = proc_ctx.ctx->module->getOrInsertGlobal(typeinfo_name, typeinfo_struct_type);
      if(auto *gv = llvm::dyn_cast<llvm::GlobalVariable>(jankExceptionTypeInfoConstant))
      {
        gv->setLinkage(llvm::GlobalValue::ExternalLinkage); // Ensure it's external
        gv->setInitializer(nullptr); // It's an external symbol, no initializer here
      }
      lp->addClause(jankExceptionTypeInfoConstant);


      // If there's a finally block, the landingpad must be a cleanup.
      if(expr->finally_body.is_some())
      {
        lp->setCleanup(true);
      }


      llvm::Value *exception_raw_ptr = builder.CreateExtractValue(lp, 0, "exc.ptr");
      // The type of jank_exception*
      llvm::Type *jank_exception_ptr_type = llvm::PointerType::getUnqual(*proc_ctx.ctx->llvm_ctx);
      llvm::Value *typed_exception_ptr
        = builder.CreateBitCast(exception_raw_ptr, jank_exception_ptr_type, "typed.exc.ptr");

      llvm::FunctionType *getJankObjFnTy
        = llvm::FunctionType::get(builder.getPtrTy(), { jank_exception_ptr_type }, false);
      llvm::FunctionCallee getJankObjFn
        = proc_ctx.ctx->module->getOrInsertFunction("jank_get_obj_from_jank_exception_c_abi",
                                                    getJankObjFnTy);
      llvm::Value *caught_jank_exception_obj
        = builder.CreateCall(getJankObjFn, { typed_exception_ptr }, "caught.jank.obj");

      llvm::BasicBlock *last_bb_of_this_path = blocks.landing_pad_bb;
      llvm::Value *result_from_catch_block
        = proc_ctx.gen_global(runtime::jank_nil); // Default if no catch

      if(expr->catch_body.is_some())
      {
        // If there's a catch body, landing pad branches to it.
        builder.CreateBr(blocks.catch_body_entry_bb);
        builder.SetInsertPoint(blocks.catch_body_entry_bb);

        auto const &catch_clause = expr->catch_body.unwrap();

        // Temporarily bind the exception object to the catch variable symbol
        llvm::Value *old_e_val_in_scope = nullptr;
        bool e_was_in_scope = proc_ctx.locals.count(catch_clause.sym);
        if(e_was_in_scope)
        {
          old_e_val_in_scope = proc_ctx.locals[catch_clause.sym];
        }
        proc_ctx.locals[catch_clause.sym] = caught_jank_exception_obj;

        // Generate catch body
        for(size_t i = 0; i < catch_clause.body->values.size(); ++i)
        {
          if(builder.GetInsertBlock()->getTerminator())
          {
            break;
          } // Path terminated

          auto const current_expr_in_catch = catch_clause.body->values[i];
          // bool is_last_in_catch = (i == catch_clause.body->values.size() - 1); // Not used directly for branching here

          // Exceptions from catch body are not caught by this try's landingpad.
          // They would propagate to an outer try or terminate.
          result_from_catch_block = proc_ctx.gen(current_expr_in_catch, caller_arity);
        }

        // After catch body, if not terminated, branch to finally (if exists) or continue block
        if(builder.GetInsertBlock()->getTerminator() == nullptr)
        {
          llvm::BasicBlock *dest_after_catch = expr->finally_body.is_some()
            ? blocks.finally_normal_path_entry_bb
            : blocks.try_continue_bb;
          if(!dest_after_catch->getParent())
          {
            current_llvm_fn->insert(current_llvm_fn->end(), dest_after_catch);
            // current_llvm_fn->getBasicBlockList().push_back(dest_after_catch);
          }
          builder.CreateBr(dest_after_catch);
        }


        // Restore original scope for the catch variable symbol
        if(e_was_in_scope)
        {
          proc_ctx.locals[catch_clause.sym] = old_e_val_in_scope;
        }
        else
        {
          proc_ctx.locals.erase(catch_clause.sym);
        }
        last_bb_of_this_path = builder.GetInsertBlock();
      }
      else // No catch body
      {
        // If no catch, but finally exists, landing pad goes to actual finally code for cleanup.
        // If no catch AND no finally, landing pad should just resume.
        if(expr->finally_body.is_some())
        {
          jank_assert_throw(blocks.finally_actual_code_bb
                            && "finally_actual_code_bb must exist if finally body is present");
          builder.CreateBr(
            blocks.finally_actual_code_bb); // Directly to actual finally code for cleanup
        }
        else
        {
          jank_assert_throw(lp && "LandingPadInst must exist if we are in landing_pad_bb");
          builder.CreateResume(lp);
        }
        last_bb_of_this_path = builder.GetInsertBlock();
      }
      return { result_from_catch_block, last_bb_of_this_path };
    }

    void gen_finally_block(
      llvm_processor &proc_ctx,
      analyze::expr::try_ref const expr,
      analyze::expr::function_arity const &caller_arity,
      TryGenBlocks &blocks,
      llvm::Value *landing_pad_inst_val, /* This is the LandingPadInst* itself, cast to Value* */
      llvm::BasicBlock
        * /* from_try_normal_final_bb_or_catch_normal_final_bb - Not directly needed for PHI this way*/
      ,
      llvm::BasicBlock *from_landingpad_direct_to_finally_bb)
    {
      if(!expr->finally_body.is_some())
      {
        return;
      }

      auto &builder = *proc_ctx.ctx->builder;
      llvm::Function *current_llvm_fn = proc_ctx.fn;

      jank_assert_throw(blocks.finally_actual_code_bb
                        && "finally_actual_code_bb must exist for finally block");
      jank_assert_throw(blocks.finally_normal_path_entry_bb
                        && "finally_normal_path_entry_bb must exist for finally block");

      // 1. Ensure `finally_normal_path_entry_bb` is correctly terminated.
      if(blocks.finally_normal_path_entry_bb->getTerminator() == nullptr)
      {
        builder.SetInsertPoint(blocks.finally_normal_path_entry_bb);
        if(llvm::pred_empty(blocks.finally_normal_path_entry_bb)
           && blocks.finally_normal_path_entry_bb != &current_llvm_fn->getEntryBlock())
        {
          builder.CreateUnreachable();
        }
        else
        {
          builder.CreateBr(blocks.finally_actual_code_bb);
        }
      }

      // 2. Set insert point to the block that contains the actual finally logic.
      builder.SetInsertPoint(blocks.finally_actual_code_bb);

      // 3. Check if `finally_actual_code_bb` is reachable.
      if(llvm::pred_empty(blocks.finally_actual_code_bb)
         && blocks.finally_actual_code_bb != &current_llvm_fn->getEntryBlock())
      {
        if(blocks.finally_actual_code_bb->getTerminator() == nullptr)
        {
          builder.CreateUnreachable();
        }
        if(blocks.rethrow_bb && blocks.rethrow_bb->getTerminator() == nullptr
           && llvm::pred_empty(blocks.rethrow_bb))
        {
          if(!blocks.rethrow_bb->getParent())
          {
            current_llvm_fn->insert(current_llvm_fn->end(), blocks.rethrow_bb);
            // current_llvm_fn->getBasicBlockList().push_back(blocks.rethrow_bb);
          }
          builder.SetInsertPoint(blocks.rethrow_bb);
          builder.CreateUnreachable();
        }
        if(blocks.try_continue_bb && blocks.try_continue_bb->getParent()
           && blocks.try_continue_bb->getTerminator() == nullptr
           && llvm::pred_empty(blocks.try_continue_bb))
        {
          if(!blocks.try_continue_bb->getParent())
          {
            current_llvm_fn->insert(current_llvm_fn->end(), blocks.try_continue_bb);
            // current_llvm_fn->getBasicBlockList().push_back(blocks.try_continue_bb);
          }
          builder.SetInsertPoint(blocks.try_continue_bb);
          builder.CreateUnreachable();
        }
        return;
      }

      // 4. Create the PHI node for `must_rethrow` at the beginning of `finally_actual_code_bb`.
      llvm::PHINode *must_rethrow_phi
        = builder.CreatePHI(builder.getInt1Ty(),
                            llvm::pred_size(blocks.finally_actual_code_bb),
                            "finally.mustrethrow.phi");

      for(llvm::BasicBlock *pred : llvm::predecessors(blocks.finally_actual_code_bb))
      {
        if(pred == blocks.finally_normal_path_entry_bb)
        { // Normal path from try or catch
          must_rethrow_phi->addIncoming(builder.getFalse(), pred);
        }
        else if(pred == from_landingpad_direct_to_finally_bb)
        { // Path from landingpad after no catch/catch handled
          must_rethrow_phi->addIncoming(builder.getTrue(), pred);
        }
        else
        {
          // This case could occur if landing_pad_bb (cleanup path) branched directly here
          // without going through `from_landingpad_direct_to_finally_bb` (which implies catch was skipped).
          // This typically means it's an exceptional path.
          must_rethrow_phi->addIncoming(builder.getTrue(), pred);
        }
      }

      if(must_rethrow_phi->getNumIncomingValues() == 0
         && !llvm::pred_empty(blocks.finally_actual_code_bb))
      {
        // Fallback if predecessors are complex and not caught by specific checks above.
        // This implies all paths leading here are exceptional if not `finally_normal_path_entry_bb`.
        for(llvm::BasicBlock *pred : llvm::predecessors(blocks.finally_actual_code_bb))
        {
          must_rethrow_phi->addIncoming(builder.getTrue(), pred);
        }
      }


      // 5. Generate the user's `finally` body.
      bool old_is_in_try_block_finally = proc_ctx.m_is_in_try_block_for_invoke;
      llvm::BasicBlock *old_unwind_dest_finally = proc_ctx.m_current_unwind_dest_for_invoke;
      proc_ctx.m_is_in_try_block_for_invoke = false;
      proc_ctx.m_current_unwind_dest_for_invoke = nullptr;

      for(auto const current_expr_in_finally : expr->finally_body.unwrap()->values)
      {
        if(builder.GetInsertBlock()->getTerminator())
        {
          break;
        }
        proc_ctx.gen(current_expr_in_finally, caller_arity);
      }

      proc_ctx.m_is_in_try_block_for_invoke = old_is_in_try_block_finally;
      proc_ctx.m_current_unwind_dest_for_invoke = old_unwind_dest_finally;

      // 6. Branch based on `must_rethrow_phi`.
      if(builder.GetInsertBlock()->getTerminator() == nullptr)
      {
        jank_assert_throw(blocks.try_continue_bb && "try_continue_bb must exist");
        if(!blocks.try_continue_bb->getParent())
        { // Ensure it's added if not yet parented
          current_llvm_fn->insert(current_llvm_fn->end(), blocks.try_continue_bb);
          // current_llvm_fn->getBasicBlockList().push_back(blocks.try_continue_bb);
        }

        if(blocks.rethrow_bb)
        {
          jank_assert_throw(landing_pad_inst_val
                            && "landing_pad_inst_val must be valid if rethrow_bb exists");
          if(!blocks.rethrow_bb->getParent())
          {
            current_llvm_fn->insert(current_llvm_fn->end(), blocks.rethrow_bb);
            // current_llvm_fn->getBasicBlockList().push_back(blocks.rethrow_bb);
          }
          builder.CreateCondBr(must_rethrow_phi, blocks.rethrow_bb, blocks.try_continue_bb);
        }
        else
        {
          builder.CreateBr(blocks.try_continue_bb);
        }
      }

      // 7. Terminate `rethrow_bb` if it exists.
      if(blocks.rethrow_bb)
      {
        if(!blocks.rethrow_bb->getParent())
        {
          current_llvm_fn->insert(current_llvm_fn->end(), blocks.rethrow_bb);
          // current_llvm_fn->getBasicBlockList().push_back(blocks.rethrow_bb);
        }
        builder.SetInsertPoint(blocks.rethrow_bb);
        if(blocks.rethrow_bb->getTerminator() == nullptr)
        {
          jank_assert_throw(landing_pad_inst_val && "Landingpad value must be provided to rethrow");
          builder.CreateResume(landing_pad_inst_val);
        }
      }
    }

    llvm::Value *gen_try_continue_block(llvm_processor &proc_ctx,
                                        analyze::expr::try_ref const expr,
                                        TryGenBlocks &blocks,
                                        llvm::AllocaInst *result_alloca,
                                        llvm::Value *try_body_result_val_direct,
                                        llvm::BasicBlock *try_path_pred_to_cont,
                                        llvm::Value *catch_body_result_val_direct,
                                        llvm::BasicBlock *catch_path_pred_to_cont)
    {
      auto &builder = *proc_ctx.ctx->builder;
      llvm::Function *current_llvm_fn = proc_ctx.fn;

      if(!blocks.try_continue_bb)
      {
        jank_debug_assert_throw(false && "try_continue_bb is null");
        return proc_ctx.gen_global(runtime::jank_nil);
      }

      if(!blocks.try_continue_bb->getParent())
      {
        current_llvm_fn->insert(current_llvm_fn->end(), blocks.try_continue_bb);
        // current_llvm_fn->getBasicBlockList().push_back(blocks.try_continue_bb);
      }
      builder.SetInsertPoint(blocks.try_continue_bb);

      if(llvm::pred_empty(blocks.try_continue_bb)
         && blocks.try_continue_bb != &current_llvm_fn->getEntryBlock())
      {
        if(blocks.try_continue_bb->getTerminator() == nullptr)
        {
          builder.CreateUnreachable();
        }
        return nullptr;
      }

      llvm::Value *final_value;
      if(result_alloca)
      {
        final_value = builder.CreateLoad(builder.getPtrTy(), result_alloca, "try.res.load");
      }
      else
      {
        unsigned num_preds = 0;
        if(try_path_pred_to_cont
           && llvm::is_contained(llvm::predecessors(blocks.try_continue_bb), try_path_pred_to_cont))
        {
          num_preds++;
        }
        if(expr->catch_body.is_some() && catch_path_pred_to_cont
           && llvm::is_contained(llvm::predecessors(blocks.try_continue_bb),
                                 catch_path_pred_to_cont))
        {
          num_preds++;
        }

        if(num_preds == 0 && !llvm::pred_empty(blocks.try_continue_bb))
        { // If block is reachable but specific preds aren't identified
          num_preds = llvm::pred_size(blocks.try_continue_bb);
        }


        llvm::PHINode *phi = builder.CreatePHI(builder.getPtrTy(), num_preds, "try.phi.no.finally");

        if(try_path_pred_to_cont
           && llvm::is_contained(llvm::predecessors(blocks.try_continue_bb), try_path_pred_to_cont))
        {
          phi->addIncoming(try_body_result_val_direct ? try_body_result_val_direct
                                                      : proc_ctx.gen_global(runtime::jank_nil),
                           try_path_pred_to_cont);
        }
        if(expr->catch_body.is_some() && catch_path_pred_to_cont
           && llvm::is_contained(llvm::predecessors(blocks.try_continue_bb),
                                 catch_path_pred_to_cont))
        {
          phi->addIncoming(catch_body_result_val_direct ? catch_body_result_val_direct
                                                        : proc_ctx.gen_global(runtime::jank_nil),
                           catch_path_pred_to_cont);
        }

        if(phi->getNumIncomingValues() == 0 && !llvm::pred_empty(blocks.try_continue_bb))
        {
          for(llvm::BasicBlock *pred : llvm::predecessors(blocks.try_continue_bb))
          {
            phi->addIncoming(proc_ctx.gen_global(runtime::jank_nil),
                             pred); // Default if specific value unknown for this path
          }
        }
        else if(phi->getNumIncomingValues() == 0 && llvm::pred_empty(blocks.try_continue_bb))
        {
          // Already handled by unreachable check at start
        }
        final_value = phi;
      }

      if(expr->position == analyze::expression_position::tail)
      {
        if(builder.GetInsertBlock()->getTerminator() == nullptr)
        {
          builder.CreateRet(final_value ? final_value : proc_ctx.gen_global(runtime::jank_nil));
        }
        return nullptr;
      }
      return final_value ? final_value : proc_ctx.gen_global(runtime::jank_nil);
    }


  } /* namespace detail */

  llvm::Value *gen_native_try_catch_finally(llvm_processor &proc_ctx,
                                            analyze::expr::try_ref const expr,
                                            analyze::expr::function_arity const &caller_arity)
  {
    jank::profile::timer const timer{ "codegen::gen_native_try_catch_finally" };
    llvm::Function *current_llvm_fn = proc_ctx.fn; // Use the already set proc_ctx.fn
    jank_assert_throw(current_llvm_fn
                      && "llvm_processor::fn is not set before gen_native_try_catch_finally");
    auto &builder = *proc_ctx.ctx->builder;

    detail::TryGenBlocks blocks
      = detail::create_try_catch_finally_blocks(proc_ctx, expr, current_llvm_fn);

    llvm::AllocaInst *result_alloca = nullptr;
    if(expr->position != analyze::expression_position::tail && expr->finally_body.is_some())
    {
      llvm::IRBuilder<>::InsertPointGuard guard(builder); // RAII for insert point
      builder.SetInsertPoint(&current_llvm_fn->getEntryBlock(),
                             current_llvm_fn->getEntryBlock().getFirstInsertionPt());
      result_alloca = builder.CreateAlloca(builder.getPtrTy(), nullptr, "try.catch.res.alloca");
      builder.CreateStore(proc_ctx.gen_global(runtime::jank_nil), result_alloca); // Initialize
    }

    llvm::BasicBlock *original_block_before_try = builder.GetInsertBlock();
    if(original_block_before_try && original_block_before_try->getTerminator() == nullptr)
    {
      builder.CreateBr(blocks.try_body_entry_bb);
    }

    bool old_is_in_try_block_for_invoke = proc_ctx.m_is_in_try_block_for_invoke;
    llvm::BasicBlock *old_unwind_dest_for_invoke = proc_ctx.m_current_unwind_dest_for_invoke;
    proc_ctx.m_is_in_try_block_for_invoke = true;
    proc_ctx.m_current_unwind_dest_for_invoke
      = blocks.landing_pad_bb; // Correct unwind for invokes inside try

    auto [try_body_last_val, try_body_final_bb_after_try_logic]
      = detail::gen_try_block_body(proc_ctx, expr, caller_arity, blocks);

    // Store try_body_last_val if alloca is used (i.e., if finally exists)
    if(result_alloca && try_body_last_val && try_body_final_bb_after_try_logic)
    {
      llvm::Instruction *terminator = try_body_final_bb_after_try_logic->getTerminator();
      if(terminator && // Must have a terminator to insert before
         (llvm::is_contained(llvm::successors(try_body_final_bb_after_try_logic),
                             blocks.finally_normal_path_entry_bb)
          || (!expr->finally_body.is_some()
              && llvm::is_contained(llvm::successors(try_body_final_bb_after_try_logic),
                                    blocks.try_continue_bb))))
      {
        llvm::IRBuilder<>::InsertPointGuard ipg(builder);
        builder.SetInsertPoint(terminator); // Insert *before* the branch
        builder.CreateStore(try_body_last_val, result_alloca);
      }
      else if(terminator == nullptr && try_body_final_bb_after_try_logic->getParent())
      {
        // If block is live but not terminated, means gen_try_block_body ended it.
        // Store might still be needed if it's implicitly falling through to a path that needs it.
        // This scenario should be rare if gen_try_block_body ensures termination.
        llvm::IRBuilder<>::InsertPointGuard ipg(builder);
        builder.SetInsertPoint(try_body_final_bb_after_try_logic);
        builder.CreateStore(try_body_last_val, result_alloca);
      }
    }


    proc_ctx.m_is_in_try_block_for_invoke = old_is_in_try_block_for_invoke;
    proc_ctx.m_current_unwind_dest_for_invoke = old_unwind_dest_for_invoke;

    llvm::LandingPadInst *landing_pad_instruction = nullptr;
    auto [catch_body_last_val, catch_body_final_bb_after_catch_logic]
      = detail::gen_landing_pad_and_catch_block(proc_ctx,
                                                expr,
                                                caller_arity,
                                                blocks,
                                                &landing_pad_instruction);

    if(result_alloca && catch_body_last_val && catch_body_final_bb_after_catch_logic)
    {
      llvm::Instruction *terminator = catch_body_final_bb_after_catch_logic->getTerminator();
      if(terminator
         && (llvm::is_contained(llvm::successors(catch_body_final_bb_after_catch_logic),
                                blocks.finally_normal_path_entry_bb)
             || (!expr->finally_body.is_some()
                 && llvm::is_contained(llvm::successors(catch_body_final_bb_after_catch_logic),
                                       blocks.try_continue_bb))))
      {
        llvm::IRBuilder<>::InsertPointGuard ipg(builder);
        builder.SetInsertPoint(terminator);
        builder.CreateStore(catch_body_last_val, result_alloca);
      }
      else if(terminator == nullptr && catch_body_final_bb_after_catch_logic->getParent())
      {
        llvm::IRBuilder<>::InsertPointGuard ipg(builder);
        builder.SetInsertPoint(catch_body_final_bb_after_catch_logic);
        builder.CreateStore(catch_body_last_val, result_alloca);
      }
    }

    llvm::BasicBlock *landingpad_pred_to_finally_actual = nullptr;
    if(blocks.landing_pad_bb && blocks.landing_pad_bb->getTerminator())
    {
      llvm::Instruction *term = blocks.landing_pad_bb->getTerminator();
      for(unsigned i = 0; i < term->getNumSuccessors(); ++i)
      {
        if(term->getSuccessor(i) == blocks.finally_actual_code_bb)
        {
          landingpad_pred_to_finally_actual = blocks.landing_pad_bb;
          break;
        }
      }
    }


    detail::gen_finally_block(proc_ctx,
                              expr,
                              caller_arity,
                              blocks,
                              landing_pad_instruction, /* Pass the actual LP instruction */
                              nullptr, /* from_try_normal_final_bb - now handled by PHI logic */
                              landingpad_pred_to_finally_actual);


    llvm::BasicBlock *pred_from_try_path_to_cont = nullptr;
    llvm::BasicBlock *pred_from_catch_path_to_cont = nullptr;

    if(expr->finally_body.is_some())
    {
      if(blocks.finally_actual_code_bb && blocks.finally_actual_code_bb->getTerminator())
      {
        llvm::Instruction *term = blocks.finally_actual_code_bb->getTerminator();
        for(unsigned i = 0; i < term->getNumSuccessors(); ++i)
        {
          if(term->getSuccessor(i) == blocks.try_continue_bb)
          {
            // If finally branches to continue, it becomes the predecessor for both try/catch paths
            pred_from_try_path_to_cont = blocks.finally_actual_code_bb;
            pred_from_catch_path_to_cont = blocks.finally_actual_code_bb;
            break;
          }
        }
      }
    }
    else
    { // No finally block
      if(try_body_final_bb_after_try_logic && try_body_final_bb_after_try_logic->getTerminator())
      {
        if(llvm::is_contained(llvm::successors(try_body_final_bb_after_try_logic),
                              blocks.try_continue_bb))
        {
          pred_from_try_path_to_cont = try_body_final_bb_after_try_logic;
        }
      }
      if(catch_body_final_bb_after_catch_logic
         && catch_body_final_bb_after_catch_logic->getTerminator())
      {
        if(llvm::is_contained(llvm::successors(catch_body_final_bb_after_catch_logic),
                              blocks.try_continue_bb))
        {
          pred_from_catch_path_to_cont = catch_body_final_bb_after_catch_logic;
        }
      }
    }

    return detail::gen_try_continue_block(proc_ctx,
                                          expr,
                                          blocks,
                                          result_alloca,
                                          try_body_last_val, // Direct val from try
                                          pred_from_try_path_to_cont,
                                          catch_body_last_val, // Direct val from catch
                                          pred_from_catch_path_to_cont);
  }

} /* namespace jank::codegen */

// END OF MODIFIED FILE: src/cpp/jank/codegen/llvm_try_catch_finally.cpp
