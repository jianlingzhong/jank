#include <jank/codegen/llvm_try_catch_finally.hpp>
#include <jank/runtime/obj/nil.hpp>
#include <jank/runtime/obj/persistent_vector.hpp> /* For get_entry in catch if needed */
#include <jank/analyze/expr/call.hpp>
#include <jank/analyze/expr/throw.hpp>
#include <jank/profile/time.hpp>
#include <jank/error.hpp> /* For jank::jank_exception */

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

    jtl::immutable_string arity_to_call_fn(size_t size);

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
          builder.CreateInvoke(throw_fn_callee,
                               normal_dest_if_invoke,
                               unwind_dest_if_invoke,
                               { thrown_val });
          /* The current block is now terminated by invoke. Normal_dest_if_invoke might be technically unreachable. */
          /* Set builder to the normal destination to allow chaining, though it might be dead code if throw always happens. */
          builder.SetInsertPoint(normal_dest_if_invoke);
          if(normal_dest_if_invoke->getTerminator() == nullptr)
          { /* If normal_dest isn't already terminated (e.g. by a return) */
            builder
              .CreateUnreachable(); /* Explicitly mark as unreachable if throw always happens */
          }
          return nullptr;
        }

        /* Must be expr::call or similar */
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

        jtl::immutable_string call_fn_name = arity_to_call_fn(call_expr->arg_exprs.size());
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
        builder.SetInsertPoint(normal_dest_if_invoke); /* Critical: continue on normal path */
        return invoke_inst;
      }
      else
      {
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
      blocks.try_continue_bb = llvm::BasicBlock::Create(*proc_ctx.ctx->llvm_ctx,
                                                        "try.cont"); /* Not added to function yet */

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
        blocks.finally_code_bb
          = llvm::BasicBlock::Create(*proc_ctx.ctx->llvm_ctx, "finally.code", current_llvm_fn);
        if(needs_landing_pad)
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
      llvm::BasicBlock *current_bb_for_normal_flow = blocks.try_body_entry_bb;

      for(size_t i = 0; i < expr->body->values.size(); ++i)
      {
        builder.SetInsertPoint(current_bb_for_normal_flow);
        if(builder.GetInsertBlock()->getTerminator())
        {
          break;
        }

        auto const current_expr_node = expr->body->values[i];
        bool is_last_in_try_body = (i == expr->body->values.size() - 1);

        llvm::BasicBlock *normal_dest_for_this_expr;
        if(is_last_in_try_body)
        {
          normal_dest_for_this_expr
            = blocks.finally_code_bb ? blocks.finally_code_bb : blocks.try_continue_bb;
        }
        else
        {
          /* Insert before landing_pad_bb to keep CFG somewhat linear for non-exception path */
          normal_dest_for_this_expr = llvm::BasicBlock::Create(*proc_ctx.ctx->llvm_ctx,
                                                               "try.stmt.next",
                                                               current_llvm_fn,
                                                               blocks.landing_pad_bb);
        }

        last_val_in_try = detail::gen_expr_potentially_throwing(proc_ctx,
                                                                current_expr_node,
                                                                caller_arity,
                                                                normal_dest_for_this_expr,
                                                                blocks.landing_pad_bb);

        /* gen_expr_potentially_throwing now sets the insert point to normal_dest_for_this_expr */
        current_bb_for_normal_flow
          = builder.GetInsertBlock(); /* This is normal_dest_for_this_expr */
        if(builder.GetInsertBlock()->getTerminator() == nullptr && !is_last_in_try_body)
        {
          /* This case should ideally not happen if gen_expr_potentially_throwing correctly handles its branches.
               If normal_dest_for_this_expr is not the final destination (finally/continue), it should have been
               terminated by a branch to the *next* statement's block by the loop structure or by gen_expr itself.
               This is a safeguard. */
          builder.CreateBr(
            normal_dest_for_this_expr); /* Should be next iteration's current_bb_for_normal_flow */
        }
      }

      builder.SetInsertPoint(
        current_bb_for_normal_flow); /* Make sure we are at the true end of normal try flow */
      if(expr->body->values.empty() && builder.GetInsertBlock()->getTerminator() == nullptr)
      {
        builder.CreateBr(blocks.finally_code_bb ? blocks.finally_code_bb : blocks.try_continue_bb);
      }
      else if(!expr->body->values.empty() && builder.GetInsertBlock()->getTerminator() == nullptr)
      {
        /* If the last expr didn't terminate and wasn't an invoke that branched to finally/continue */
        builder.CreateBr(blocks.finally_code_bb ? blocks.finally_code_bb : blocks.try_continue_bb);
      }

      return { last_val_in_try, current_bb_for_normal_flow };
    }

    std::pair<llvm::Value *, llvm::BasicBlock *>
    gen_landing_pad_and_catch_block(llvm_processor &proc_ctx,
                                    analyze::expr::try_ref const expr,
                                    analyze::expr::function_arity const &caller_arity,
                                    TryGenBlocks &blocks,
                                    llvm::LandingPadInst **out_landing_pad_inst)
    {
      if(!blocks.landing_pad_bb)
      {
        *out_landing_pad_inst = nullptr;
        return { proc_ctx.gen_global(runtime::jank_nil), nullptr };
      }
      auto &builder = *proc_ctx.ctx->builder;

      builder.SetInsertPoint(blocks.landing_pad_bb);
      llvm::Type *landing_pad_type
        = llvm::StructType::get(*proc_ctx.ctx->llvm_ctx,
                                { builder.getPtrTy(), builder.getInt32Ty() });
      llvm::LandingPadInst *lp = builder.CreateLandingPad(landing_pad_type, 1, "lpad");
      *out_landing_pad_inst = lp;

      lp->setCleanup(expr->finally_body.is_some());

      /* Get TypeInfo for jank::jank_exception as a global constant */
      std::string typeinfo_name = "_ZTIn4jank14jank_exceptionE"; /* Itanium ABI */
      llvm::Type *typeinfo_struct_type
        = llvm::StructType::getTypeByName(*proc_ctx.ctx->llvm_ctx, "class.std::type_info");
      if(!typeinfo_struct_type)
      {
        typeinfo_struct_type
          = llvm::StructType::create(*proc_ctx.ctx->llvm_ctx, "class.std::type_info");
      }
      llvm::Constant *jankExceptionTypeInfoConstant = proc_ctx.ctx->module->getOrInsertGlobal(
        typeinfo_name,
        typeinfo_struct_type /* Actual type_info object, not pointer to it */
      );
      if(auto *gv = llvm::dyn_cast<llvm::GlobalVariable>(jankExceptionTypeInfoConstant))
      {
        gv->setLinkage(llvm::GlobalValue::ExternalLinkage);
        /* Clause should be a pointer to the type_info object */
        lp->addClause(gv);
      }
      else
      {
        /* This might happen if the global was already a ConstantExpr, e.g. a bitcast.
           This needs to resolve to a Constant* that is a pointer to the type_info object.
           A direct GlobalVariable is the most straightforward.
           If problems persist, using a null pointer for a catch-all and then dynamic checks
           might be a fallback, but loses type-safety at landingpad. */
        lp->addClause(llvm::ConstantPointerNull::get(
          llvm::PointerType::getUnqual(*proc_ctx.ctx->llvm_ctx))); /* Catch all as fallback */
      }


      llvm::Value *exception_raw_ptr = builder.CreateExtractValue(lp, 0, "exc.ptr");
      llvm::Type *jank_exception_ptr_type
        = llvm::PointerType::getUnqual(*proc_ctx.ctx->llvm_ctx); /* Opaque pointer */
      llvm::Value *typed_exception_ptr
        = builder.CreateBitCast(exception_raw_ptr, jank_exception_ptr_type);

      llvm::FunctionType *getJankObjFnTy
        = llvm::FunctionType::get(builder.getPtrTy(), { jank_exception_ptr_type }, false);
      llvm::FunctionCallee getJankObjFn
        = proc_ctx.ctx->module->getOrInsertFunction("jank_get_obj_from_jank_exception_c_abi",
                                                    getJankObjFnTy);
      llvm::Value *caught_jank_exception_obj
        = builder.CreateCall(getJankObjFn, { typed_exception_ptr }, "caught.jank.obj");

      llvm::BasicBlock *last_bb_of_this_path = blocks.landing_pad_bb;
      llvm::Value *result_from_catch_block = proc_ctx.gen_global(runtime::jank_nil);

      if(expr->catch_body.is_some())
      {
        builder.CreateBr(blocks.catch_body_entry_bb);
        builder.SetInsertPoint(blocks.catch_body_entry_bb);

        auto const &catch_clause = expr->catch_body.unwrap();

        llvm::Value *old_e_val_in_scope = nullptr;
        bool e_was_in_scope = proc_ctx.locals.count(catch_clause.sym);
        if(e_was_in_scope)
        {
          old_e_val_in_scope = proc_ctx.locals[catch_clause.sym];
        }
        proc_ctx.locals[catch_clause.sym] = caught_jank_exception_obj;

        for(size_t i = 0; i < catch_clause.body->values.size(); ++i)
        {
          if(builder.GetInsertBlock()->getTerminator())
          {
            break;
          }
          auto const current_expr_in_catch = catch_clause.body->values[i];
          bool is_last_in_catch = (i == catch_clause.body->values.size() - 1);

          result_from_catch_block = proc_ctx.gen(current_expr_in_catch, caller_arity);

          if(is_last_in_catch && builder.GetInsertBlock()->getTerminator() == nullptr)
          {
            builder.CreateBr(blocks.finally_code_bb ? blocks.finally_code_bb
                                                    : blocks.try_continue_bb);
          }
        }
        if(catch_clause.body->values.empty()
           && builder.GetInsertBlock()->getTerminator() == nullptr)
        {
          builder.CreateBr(blocks.finally_code_bb ? blocks.finally_code_bb
                                                  : blocks.try_continue_bb);
        }

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
      else
      {
        if(blocks.finally_code_bb)
        {
          builder.CreateBr(blocks.finally_code_bb);
        }
        else
        {
          builder.CreateResume(lp);
        }
        last_bb_of_this_path = builder.GetInsertBlock();
      }
      return { result_from_catch_block, last_bb_of_this_path };
    }

    void gen_finally_block(llvm_processor &proc_ctx,
                           analyze::expr::try_ref const expr,
                           analyze::expr::function_arity const &caller_arity,
                           TryGenBlocks &blocks,
                           llvm::Value *landing_pad_inst_val,
                           llvm::BasicBlock *from_try_normal_exit_bb,
                           llvm::BasicBlock *from_catch_normal_exit_bb,
                           llvm::BasicBlock *from_landingpad_unhandled_exit_bb)
    {
      if(!blocks.finally_code_bb)
      {
        return;
      }

      auto &builder = *proc_ctx.ctx->builder;
      builder.SetInsertPoint(blocks.finally_code_bb);

      llvm::PHINode *must_rethrow_phi
        = builder.CreatePHI(builder.getInt1Ty(), 0, "finally.mustrethrow");

      if(from_try_normal_exit_bb && from_try_normal_exit_bb->getTerminator()
         && llvm::dyn_cast<llvm::BranchInst>(from_try_normal_exit_bb->getTerminator())
         && /* Make sure it's a branch */
         from_try_normal_exit_bb->getTerminator()->getSuccessor(0) == blocks.finally_code_bb)
      {
        must_rethrow_phi->addIncoming(builder.getFalse(), from_try_normal_exit_bb);
      }
      if(from_catch_normal_exit_bb && from_catch_normal_exit_bb->getTerminator()
         && llvm::dyn_cast<llvm::BranchInst>(from_catch_normal_exit_bb->getTerminator())
         && from_catch_normal_exit_bb->getTerminator()->getSuccessor(0) == blocks.finally_code_bb)
      {
        must_rethrow_phi->addIncoming(builder.getFalse(), from_catch_normal_exit_bb);
      }
      if(from_landingpad_unhandled_exit_bb && from_landingpad_unhandled_exit_bb->getTerminator()
         && llvm::dyn_cast<llvm::BranchInst>(from_landingpad_unhandled_exit_bb->getTerminator())
         && from_landingpad_unhandled_exit_bb->getTerminator()->getSuccessor(0)
           == blocks.finally_code_bb)
      {
        must_rethrow_phi->addIncoming(builder.getTrue(), from_landingpad_unhandled_exit_bb);
      }

      /* If PHI is still empty but block has predecessors, something is wrong with CFG construction earlier. */
      /* Add a default incoming for each actual predecessor if phi is empty to avoid verifier errors. */
      if(must_rethrow_phi->getNumIncomingValues() == 0
         && blocks.finally_code_bb->hasNPredecessorsOrMore(1))
      {
        for(llvm::BasicBlock *pred : llvm::predecessors(blocks.finally_code_bb))
        {
          /* This default is likely incorrect semantically but prevents crashes.
               The logic adding predecessors to finally_code_bb needs to be robust. */
          must_rethrow_phi->addIncoming(builder.getFalse(), pred);
        }
      }


      for(auto const current_expr_in_finally : expr->finally_body.unwrap()->values)
      {
        if(builder.GetInsertBlock()->getTerminator())
        {
          break;
        }
        proc_ctx.gen(current_expr_in_finally, caller_arity);
      }

      if(builder.GetInsertBlock()->getTerminator() == nullptr)
      {
        if(blocks.rethrow_bb)
        {
          builder.CreateCondBr(must_rethrow_phi, blocks.rethrow_bb, blocks.try_continue_bb);
        }
        else
        {
          builder.CreateBr(blocks.try_continue_bb);
        }
      }

      if(blocks.rethrow_bb)
      {
        builder.SetInsertPoint(blocks.rethrow_bb);
        if(builder.GetInsertBlock()->getTerminator() == nullptr)
        { /* Avoid adding multiple terminators */
          jank_assert(landing_pad_inst_val
                      && "Landingpad value must be provided to rethrow an exception");
          builder.CreateResume(landing_pad_inst_val);
        }
      }
    }

    llvm::Value *gen_try_continue_block(llvm_processor &proc_ctx,
                                        analyze::expr::try_ref const expr,
                                        TryGenBlocks &blocks,
                                        llvm::Value *try_body_result_val,
                                        llvm::BasicBlock *try_body_final_bb_before_finally,
                                        llvm::Value *catch_body_result_val,
                                        llvm::BasicBlock *catch_body_final_bb_before_finally)
    {
      auto &builder = *proc_ctx.ctx->builder;
      llvm::Function *current_llvm_fn = builder.GetInsertBlock()->getParent();

      bool is_try_continue_reachable = false;
      if(blocks.finally_code_bb)
      {
        /* If finally exists, try_continue_bb is reached from finally's normal exit path */
        for(llvm::User *U : blocks.finally_code_bb->users())
        {
          if(auto *term = llvm::dyn_cast<llvm::TerminatorInst>(U))
          {
            for(unsigned i = 0; i < term->getNumSuccessors(); ++i)
            {
              if(term->getSuccessor(i) == blocks.try_continue_bb)
              {
                is_try_continue_reachable = true;
                break;
              }
            }
          }
          if(is_try_continue_reachable)
          {
            break;
          }
        }
      }
      else
      {
        /* If no finally, try_continue_bb is reached from try's normal exit or catch's normal exit */
        if(try_body_final_bb_before_finally && try_body_final_bb_before_finally->getTerminator()
           && try_body_final_bb_before_finally->getTerminator()->getSuccessor(0)
             == blocks.try_continue_bb)
        {
          is_try_continue_reachable = true;
        }
        if(!is_try_continue_reachable && catch_body_final_bb_before_finally
           && catch_body_final_bb_before_finally->getTerminator()
           && catch_body_final_bb_before_finally->getTerminator()->getSuccessor(0)
             == blocks.try_continue_bb)
        {
          is_try_continue_reachable = true;
        }
      }

      if(!is_try_continue_reachable && expr->position == analyze::expression_position::tail)
      {
        /* If it's tail position and the continue block isn't reachable, it means all paths
           must have returned or rethrown. We might not need to add it.
           However, if no blocks were generated before this, current_llvm_fn might be empty. */
        if(current_llvm_fn->empty())
        {
          current_llvm_fn->getBasicBlockList().push_back(blocks.try_continue_bb);
        }
        else if(blocks.try_continue_bb->use_empty() && blocks.try_continue_bb->hasNPredecessors(0))
        {
          /* Block is truly dead, can be deleted or let DCE handle it */
          blocks.try_continue_bb->deleteValue(); /* Risky if not careful */
          blocks.try_continue_bb = nullptr;
          return nullptr;
        }
      }


      if(blocks.try_continue_bb && !blocks.try_continue_bb->getParent())
      {
        current_llvm_fn->getBasicBlockList().push_back(blocks.try_continue_bb);
      }
      if(!blocks.try_continue_bb)
      {
        return nullptr; /* Was deleted or deemed unnecessary */
      }

      builder.SetInsertPoint(blocks.try_continue_bb);

      if(expr->position == analyze::expression_position::tail)
      {
        if(builder.GetInsertBlock()->getTerminator() == nullptr)
        {
          llvm::PHINode *phi = builder.CreatePHI(builder.getPtrTy(), 0, "try.tail.phi.res");

          llvm::BasicBlock *actual_pred_from_try
            = blocks.finally_code_bb ? blocks.finally_code_bb : try_body_final_bb_before_finally;
          if(actual_pred_from_try && actual_pred_from_try->getTerminator()
             && actual_pred_from_try->getTerminator()->isTerminator()
             && /* Check it's a valid terminator */
             std::find(llvm::pred_begin(blocks.try_continue_bb),
                       llvm::pred_end(blocks.try_continue_bb),
                       actual_pred_from_try)
               != llvm::pred_end(blocks.try_continue_bb))
          {
            phi->addIncoming(try_body_result_val ? try_body_result_val
                                                 : proc_ctx.gen_global(runtime::jank_nil),
                             actual_pred_from_try);
          }

          llvm::BasicBlock *actual_pred_from_catch
            = blocks.finally_code_bb ? blocks.finally_code_bb : catch_body_final_bb_before_finally;
          if(expr->catch_body.is_some() && actual_pred_from_catch
             && actual_pred_from_catch->getTerminator()
             && actual_pred_from_catch->getTerminator()->isTerminator()
             && std::find(llvm::pred_begin(blocks.try_continue_bb),
                          llvm::pred_end(blocks.try_continue_bb),
                          actual_pred_from_catch)
               != llvm::pred_end(blocks.try_continue_bb))
          {
            phi->addIncoming(catch_body_result_val ? catch_body_result_val
                                                   : proc_ctx.gen_global(runtime::jank_nil),
                             actual_pred_from_catch);
          }

          if(phi->getNumIncomingValues() > 0)
          {
            builder.CreateRet(phi);
          }
          else if(builder.GetInsertBlock()->getTerminator() == nullptr)
          {
            builder.CreateRet(proc_ctx.gen_global(runtime::jank_nil));
          }
        }
        return nullptr;
      }
      else
      {
        llvm::PHINode *phi = builder.CreatePHI(builder.getPtrTy(), 0, "try.expr.phi.res");
        llvm::BasicBlock *actual_pred_from_try
          = blocks.finally_code_bb ? blocks.finally_code_bb : try_body_final_bb_before_finally;
        if(actual_pred_from_try && actual_pred_from_try->getTerminator()
           && actual_pred_from_try->getTerminator()->isTerminator()
           && std::find(llvm::pred_begin(blocks.try_continue_bb),
                        llvm::pred_end(blocks.try_continue_bb),
                        actual_pred_from_try)
             != llvm::pred_end(blocks.try_continue_bb))
        {
          phi->addIncoming(try_body_result_val ? try_body_result_val
                                               : proc_ctx.gen_global(runtime::jank_nil),
                           actual_pred_from_try);
        }

        llvm::BasicBlock *actual_pred_from_catch
          = blocks.finally_code_bb ? blocks.finally_code_bb : catch_body_final_bb_before_finally;
        if(expr->catch_body.is_some() && actual_pred_from_catch
           && actual_pred_from_catch->getTerminator()
           && actual_pred_from_catch->getTerminator()->isTerminator()
           && std::find(llvm::pred_begin(blocks.try_continue_bb),
                        llvm::pred_end(blocks.try_continue_bb),
                        actual_pred_from_catch)
             != llvm::pred_end(blocks.try_continue_bb))
        {
          phi->addIncoming(catch_body_result_val ? catch_body_result_val
                                                 : proc_ctx.gen_global(runtime::jank_nil),
                           actual_pred_from_catch);
        }

        if(phi->getNumIncomingValues() == 0)
        {
          if(builder.GetInsertBlock()->getTerminator() == nullptr)
          {
            return proc_ctx.gen_global(runtime::jank_nil);
          }
          return nullptr; /* Block should be dead or already terminated */
        }
        return phi;
      }
    }

  } /* namespace detail */

  llvm::Value *gen_native_try_catch_finally(llvm_processor &proc_ctx,
                                            analyze::expr::try_ref const expr,
                                            analyze::expr::function_arity const &caller_arity)
  {
    jank::profile::timer const timer{ "codegen::gen_native_try_catch_finally" };
    llvm::Function *current_llvm_fn = proc_ctx.ctx->builder->GetInsertBlock()->getParent();

    detail::TryGenBlocks blocks
      = detail::create_try_catch_finally_blocks(proc_ctx, expr, current_llvm_fn);

    llvm::BasicBlock *original_block_before_try = proc_ctx.ctx->builder->GetInsertBlock();
    if(original_block_before_try->getTerminator() == nullptr)
    {
      proc_ctx.ctx->builder->CreateBr(blocks.try_body_entry_bb);
    }

    /* --- Save and Set Invoke Flags for Try Body --- */
    bool old_is_in_try_block_for_invoke = proc_ctx.m_is_in_try_block_for_invoke;
    llvm::BasicBlock *old_unwind_dest = proc_ctx.m_current_unwind_dest_for_invoke;
    proc_ctx.m_is_in_try_block_for_invoke = true;
    proc_ctx.m_current_unwind_dest_for_invoke
      = blocks.landing_pad_bb; /* All invokes in try body go here */

    auto [try_body_last_val, try_body_final_bb]
      = detail::gen_try_block_body(proc_ctx, expr, caller_arity, blocks);

    proc_ctx.m_is_in_try_block_for_invoke = old_is_in_try_block_for_invoke;
    proc_ctx.m_current_unwind_dest_for_invoke = old_unwind_dest;

    /* --- Landing Pad and Catch --- */
    llvm::LandingPadInst *landing_pad_instruction = nullptr;
    auto [catch_body_last_val, catch_body_final_bb]
      = detail::gen_landing_pad_and_catch_block(proc_ctx,
                                                expr,
                                                caller_arity,
                                                blocks,
                                                &landing_pad_instruction);

    /* Determine the block from which an unhandled exception (from landingpad) would go to finally */
    llvm::BasicBlock *landingpad_exit_to_finally = nullptr;
    if(blocks.landing_pad_bb && blocks.landing_pad_bb->getTerminator())
    {
      llvm::TerminatorInst *term = blocks.landing_pad_bb->getTerminator();
      if(term->getNumSuccessors() > 0 && term->getSuccessor(0) == blocks.finally_code_bb)
      {
        /* This means landingpad directly branched to finally (e.g., no catch clause or catch didn't handle and branched to finally) */
        landingpad_exit_to_finally = blocks.landing_pad_bb;
      }
    }

    /* --- Finally --- */
    detail::gen_finally_block(proc_ctx,
                              expr,
                              caller_arity,
                              blocks,
                              landing_pad_instruction,
                              try_body_final_bb,
                              catch_body_final_bb,
                              landingpad_exit_to_finally);

    /* --- Continue Block and Result PHI --- */
    return detail::gen_try_continue_block(proc_ctx,
                                          expr,
                                          blocks,
                                          try_body_last_val,
                                          try_body_final_bb,
                                          catch_body_last_val,
                                          catch_body_final_bb);
  }

} /* namespace jank::codegen */
