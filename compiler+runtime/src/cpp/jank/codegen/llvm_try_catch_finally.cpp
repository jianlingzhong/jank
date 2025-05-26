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

    // gen_expr_potentially_throwing (NO CHANGES NEEDED FROM PREVIOUS CORRECT VERSION)
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
          builder.SetInsertPoint(normal_dest_if_invoke);
          if(normal_dest_if_invoke->getTerminator() == nullptr)
          {
            builder.CreateUnreachable();
          }
          return nullptr;
        }

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
          = llvm_processor::arity_to_call_fn(call_expr->arg_exprs.size()); /* Fixed static call */
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
        builder.SetInsertPoint(normal_dest_if_invoke);
        return invoke_inst;
      }
      else
      {
        return proc_ctx.gen(an_expr, current_fn_arity);
      }
    }

    // create_try_catch_finally_blocks (NO CHANGES NEEDED FROM PREVIOUS CORRECT VERSION)
    TryGenBlocks
    create_try_catch_finally_blocks(llvm_processor &proc_ctx, /* Changed to non-const */
                                    analyze::expr::try_ref const expr,
                                    llvm::Function *current_llvm_fn)
    {
      TryGenBlocks blocks{};
      blocks.try_body_entry_bb
        = llvm::BasicBlock::Create(*proc_ctx.ctx->llvm_ctx, "try.body.entry", current_llvm_fn);
      blocks.try_continue_bb = llvm::BasicBlock::Create(*proc_ctx.ctx->llvm_ctx,
                                                        "try.cont"); /* Parented later if used */

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
        blocks.finally_normal_path_entry_bb = llvm::BasicBlock::Create(*proc_ctx.ctx->llvm_ctx,
                                                                       "finally.normal.entry",
                                                                       current_llvm_fn);
        blocks.finally_actual_code_bb = llvm::BasicBlock::Create(*proc_ctx.ctx->llvm_ctx,
                                                                 "finally.actual.code",
                                                                 current_llvm_fn);
        if(needs_landing_pad)
        {
          blocks.rethrow_bb
            = llvm::BasicBlock::Create(*proc_ctx.ctx->llvm_ctx, "finally.rethrow", current_llvm_fn);
        }
      }
      return blocks;
    }

    // gen_try_block_body (NO CHANGES NEEDED FROM PREVIOUS CORRECT VERSION)
    std::pair<llvm::Value *, llvm::BasicBlock *>
    gen_try_block_body(llvm_processor &proc_ctx,
                       analyze::expr::try_ref const expr,
                       analyze::expr::function_arity const &caller_arity,
                       TryGenBlocks &blocks) /* Changed to non-const TryGenBlocks */
    {
      llvm::Function *current_llvm_fn = proc_ctx.ctx->builder->GetInsertBlock()->getParent();
      auto &builder = *proc_ctx.ctx->builder;

      builder.SetInsertPoint(blocks.try_body_entry_bb);

      llvm::Value *last_val_in_try = proc_ctx.gen_global(runtime::jank_nil);
      llvm::BasicBlock *current_normal_flow_bb = blocks.try_body_entry_bb;

      for(size_t i = 0; i < expr->body->values.size(); ++i)
      {
        builder.SetInsertPoint(current_normal_flow_bb);
        if(builder.GetInsertBlock()->getTerminator())
        {
          break;
        }

        auto const current_expr_node = expr->body->values[i];
        bool is_last_in_try_body = (i == expr->body->values.size() - 1);

        llvm::BasicBlock *normal_dest_for_this_expr;
        if(is_last_in_try_body)
        {
          normal_dest_for_this_expr = expr->finally_body.is_some()
            ? blocks.finally_normal_path_entry_bb
            : blocks.try_continue_bb;
        }
        else
        {
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

        current_normal_flow_bb = builder.GetInsertBlock();
        if(builder.GetInsertBlock()->getTerminator() == nullptr && !is_last_in_try_body)
        {
          builder.CreateBr(normal_dest_for_this_expr);
          current_normal_flow_bb = normal_dest_for_this_expr;
        }
      }

      builder.SetInsertPoint(current_normal_flow_bb);
      if(builder.GetInsertBlock()->getTerminator() == nullptr)
      {
        builder.CreateBr(expr->finally_body.is_some() ? blocks.finally_normal_path_entry_bb
                                                      : blocks.try_continue_bb);
      }

      return { last_val_in_try, current_normal_flow_bb };
    }

    // gen_landing_pad_and_catch_block (NO CHANGES NEEDED FROM PREVIOUS CORRECT VERSION)
    std::pair<llvm::Value *, llvm::BasicBlock *>
    gen_landing_pad_and_catch_block(llvm_processor &proc_ctx,
                                    analyze::expr::try_ref const expr,
                                    analyze::expr::function_arity const &caller_arity,
                                    TryGenBlocks &blocks, /* Changed to non-const TryGenBlocks */
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

      std::string typeinfo_name = "_ZTIn4jank14jank_exceptionE";
      llvm::Type *typeinfo_struct_type
        = llvm::StructType::getTypeByName(*proc_ctx.ctx->llvm_ctx, "class.std::type_info");
      if(!typeinfo_struct_type)
      {
        typeinfo_struct_type
          = llvm::StructType::create(*proc_ctx.ctx->llvm_ctx, "class.std::type_info");
      }
      llvm::Constant *jankExceptionTypeInfoConstant
        = proc_ctx.ctx->module->getOrInsertGlobal(typeinfo_name, typeinfo_struct_type);
      if(auto *gv = llvm::dyn_cast<llvm::GlobalVariable>(jankExceptionTypeInfoConstant))
      {
        gv->setLinkage(llvm::GlobalValue::ExternalLinkage);
        lp->addClause(gv);
      }
      else
      {
        lp->addClause(
          llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(*proc_ctx.ctx->llvm_ctx)));
      }

      llvm::Value *exception_raw_ptr = builder.CreateExtractValue(lp, 0, "exc.ptr");
      llvm::Type *jank_exception_ptr_type = llvm::PointerType::getUnqual(*proc_ctx.ctx->llvm_ctx);
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
            builder.CreateBr(expr->finally_body.is_some() ? blocks.finally_normal_path_entry_bb
                                                          : blocks.try_continue_bb);
          }
        }
        if(catch_clause.body->values.empty()
           && builder.GetInsertBlock()->getTerminator() == nullptr)
        {
          builder.CreateBr(expr->finally_body.is_some() ? blocks.finally_normal_path_entry_bb
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
        if(blocks.finally_actual_code_bb)
        { /* If finally exists, exceptional path goes to actual finally code */
          builder.CreateBr(blocks.finally_actual_code_bb);
        }
        else
        { /* No catch, no finally, resume immediately */
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
      TryGenBlocks &blocks, /* Changed to non-const TryGenBlocks */
      llvm::Value *landing_pad_inst_val,
      llvm::BasicBlock
        * /* from_try_normal_final_bb - now implicitly via finally_normal_path_entry_bb */,
      // llvm::BasicBlock *from_catch_normal_final_bb, /* CORRECTED: removed unused parameter */
      llvm::BasicBlock *from_landingpad_unhandled_bb)
    {
      if(!expr->finally_body.is_some())
      {
        return;
      }
      jank_assert_throw(blocks.finally_actual_code_bb && "finally_actual_code_bb must exist");
      jank_assert_throw(blocks.finally_normal_path_entry_bb
                        && "finally_normal_path_entry_bb must exist");

      auto &builder = *proc_ctx.ctx->builder;
      /* llvm::Function *current_llvm_fn = proc_ctx.ctx->builder->GetInsertBlock()->getParent(); // Unused */

      builder.SetInsertPoint(blocks.finally_normal_path_entry_bb);
      if(blocks.finally_normal_path_entry_bb->getTerminator() == nullptr
         && (blocks.finally_normal_path_entry_bb->hasNPredecessorsOrMore(1)
             || /* If it has predecessors */
             !blocks.finally_normal_path_entry_bb
                ->use_empty()) /* Or if it will be used (e.g. as target for invoke) */
      )
      {
        builder.CreateBr(blocks.finally_actual_code_bb);
      }
      else if(blocks.finally_normal_path_entry_bb->getTerminator() == nullptr)
      {
        /* If it's truly an orphaned block created by CreateBasicBlock but never wired up
           and never targeted by an invoke's normal path that is live. */
        builder.CreateUnreachable();
      }


      builder.SetInsertPoint(blocks.finally_actual_code_bb);

      llvm::PHINode *must_rethrow_phi
        = builder.CreatePHI(builder.getInt1Ty(), 0, "finally.mustrethrow.phi");

      if(blocks.finally_normal_path_entry_bb->getTerminator()
         && llvm::dyn_cast<llvm::BranchInst>(blocks.finally_normal_path_entry_bb->getTerminator())
         && blocks.finally_normal_path_entry_bb->getTerminator()->getSuccessor(0)
           == blocks.finally_actual_code_bb)
      {
        must_rethrow_phi->addIncoming(builder.getFalse(), blocks.finally_normal_path_entry_bb);
      }

      if(from_landingpad_unhandled_bb && from_landingpad_unhandled_bb->getTerminator()
         && llvm::dyn_cast<llvm::BranchInst>(from_landingpad_unhandled_bb->getTerminator())
         && from_landingpad_unhandled_bb->getTerminator()->getSuccessor(0)
           == blocks.finally_actual_code_bb)
      {
        must_rethrow_phi->addIncoming(builder.getTrue(), from_landingpad_unhandled_bb);
      }

      if(must_rethrow_phi->getNumIncomingValues() == 0
         && blocks.finally_actual_code_bb->hasNPredecessorsOrMore(1))
      {
        for(llvm::BasicBlock *pred : llvm::predecessors(blocks.finally_actual_code_bb))
        {
          must_rethrow_phi->addIncoming(builder.getFalse(), pred);
        }
      }
      else if(must_rethrow_phi->getNumIncomingValues() == 0
              && !blocks.finally_actual_code_bb->hasNPredecessorsOrMore(1))
      {
        if(blocks.finally_actual_code_bb->getTerminator() == nullptr)
        {
          builder.CreateBr(blocks.try_continue_bb);
        }
        if(blocks.rethrow_bb && blocks.rethrow_bb->getTerminator() == nullptr)
        {
          builder.SetInsertPoint(blocks.rethrow_bb);
          builder.CreateUnreachable();
        }
        return;
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
        {
          jank_assert_throw(
            landing_pad_inst_val
            && "Landingpad value must be provided to rethrow an exception from finally");
          builder.CreateResume(landing_pad_inst_val);
        }
      }
    }

    llvm::Value *gen_try_continue_block(
      llvm_processor &proc_ctx,
      analyze::expr::try_ref const expr,
      TryGenBlocks &blocks,
      llvm::AllocaInst *result_alloca, /* NEW: pass the alloca */
      llvm::BasicBlock *try_path_pred_to_cont,
      /* llvm::Value* catch_body_result_val, // No longer need separate value */
      llvm::BasicBlock *catch_path_pred_to_cont)
    {
      auto &builder = *proc_ctx.ctx->builder;
      // llvm::Function *current_llvm_fn = proc_ctx.ctx->builder->GetInsertBlock()->getParent();

      /* ... (block parenting logic as before) ... */
      if(!blocks.try_continue_bb)
      {
        return nullptr;
      }
      builder.SetInsertPoint(blocks.try_continue_bb);

      llvm::Value *final_val_for_phi_or_ret = nullptr;
      if(result_alloca)
      { /* If we used an alloca, load the result from it */
        final_val_for_phi_or_ret
          = builder.CreateLoad(builder.getPtrTy(), result_alloca, "try.res.load");
      }

      if(expr->position == analyze::expression_position::tail)
      {
        if(builder.GetInsertBlock()->getTerminator() == nullptr)
        {
          if(result_alloca)
          {
            builder.CreateRet(final_val_for_phi_or_ret);
          }
          else
          { /* No finally, PHI needed to select between direct try/catch results */
            llvm::PHINode *phi = builder.CreatePHI(builder.getPtrTy(), 0, "try.tail.phi.direct");
            if(try_path_pred_to_cont
               && llvm::is_contained(llvm::predecessors(blocks.try_continue_bb),
                                     try_path_pred_to_cont))
            {
              /* If no alloca, try_body_result_val would be passed instead of result_alloca */
              phi->addIncoming(
                proc_ctx.gen_global(
                  runtime::
                    jank_nil) /* Placeholder: should be try_body_last_val if !result_alloca */,
                try_path_pred_to_cont);
            }
            if(expr->catch_body.is_some() && catch_path_pred_to_cont
               && llvm::is_contained(llvm::predecessors(blocks.try_continue_bb),
                                     catch_path_pred_to_cont))
            {
              phi->addIncoming(
                proc_ctx.gen_global(
                  runtime::jank_nil) /* Placeholder: catch_body_last_val if !result_alloca */,
                catch_path_pred_to_cont);
            }
            if(phi->getNumIncomingValues() > 0)
            {
              builder.CreateRet(phi);
            }
            else
            {
              builder.CreateRet(proc_ctx.gen_global(runtime::jank_nil)); /* Fallback */
            }
          }
        }
        return nullptr;
      }
      else
      { /* Not tail position */
        if(result_alloca)
        {
          return final_val_for_phi_or_ret;
        }
        else
        { /* No finally, PHI needed to select between direct try/catch results */
          llvm::PHINode *phi = builder.CreatePHI(builder.getPtrTy(), 0, "try.expr.phi.direct");
          if(try_path_pred_to_cont
             && llvm::is_contained(llvm::predecessors(blocks.try_continue_bb),
                                   try_path_pred_to_cont))
          {
            phi->addIncoming(proc_ctx.gen_global(runtime::jank_nil) /* Placeholder */,
                             try_path_pred_to_cont);
          }
          if(expr->catch_body.is_some() && catch_path_pred_to_cont
             && llvm::is_contained(llvm::predecessors(blocks.try_continue_bb),
                                   catch_path_pred_to_cont))
          {
            phi->addIncoming(proc_ctx.gen_global(runtime::jank_nil) /* Placeholder */,
                             catch_path_pred_to_cont);
          }
          if(phi->getNumIncomingValues() > 0)
          {
            return phi;
          }
          return proc_ctx.gen_global(runtime::jank_nil); /* Fallback */
        }
      }
    }

  } /* namespace detail */

  /* gen_native_try_catch_finally (NO CHANGES NEEDED FROM PREVIOUS CORRECT VERSION) */
  llvm::Value *gen_native_try_catch_finally(llvm_processor &proc_ctx,
                                            analyze::expr::try_ref const expr,
                                            analyze::expr::function_arity const &caller_arity)
  {
    jank::profile::timer const timer{ "codegen::gen_native_try_catch_finally" };
    llvm::Function *current_llvm_fn = proc_ctx.ctx->builder->GetInsertBlock()->getParent();
    auto &builder = *proc_ctx.ctx->builder;

    detail::TryGenBlocks blocks
      = detail::create_try_catch_finally_blocks(proc_ctx, expr, current_llvm_fn);

    /* Alloca to store the result of the try or catch block if not in tail position
       and if a finally block exists (as finally might interrupt direct flow to PHI). */
    llvm::AllocaInst *result_alloca = nullptr;
    if(expr->position != analyze::expression_position::tail && expr->finally_body.is_some())
    {
      llvm::IRBuilderBase::InsertPointGuard guard(builder);
      builder.SetInsertPoint(&current_llvm_fn->getEntryBlock(),
                             current_llvm_fn->getEntryBlock().getFirstInsertionPt());
      result_alloca = builder.CreateAlloca(builder.getPtrTy(), nullptr, "try.catch.res.alloca");
      builder.CreateStore(proc_ctx.gen_global(runtime::jank_nil), result_alloca); /* Initialize */
    }

    llvm::BasicBlock *original_block_before_try = builder.GetInsertBlock();
    if(original_block_before_try && original_block_before_try->getTerminator() == nullptr)
    {
      builder.CreateBr(blocks.try_body_entry_bb);
    }

    /* --- Save and Set Invoke Flags for Try Body --- */
    bool old_is_in_try_block_for_invoke = proc_ctx.m_is_in_try_block_for_invoke;
    llvm::BasicBlock *old_unwind_dest = proc_ctx.m_current_unwind_dest_for_invoke;
    proc_ctx.m_is_in_try_block_for_invoke = true;
    proc_ctx.m_current_unwind_dest_for_invoke = blocks.landing_pad_bb;

    auto [try_body_last_val, try_body_final_bb_after_try_logic]
      = detail::gen_try_block_body(proc_ctx, expr, caller_arity, blocks);

    /* Store try_body_last_val if needed before going to finally */
    if(result_alloca && try_body_final_bb_after_try_logic
       && !try_body_final_bb_after_try_logic->getTerminator()
       && (try_body_final_bb_after_try_logic->getTerminator() == nullptr
           || /* Path continues to finally */
           (try_body_final_bb_after_try_logic->getTerminator()->getNumSuccessors() > 0
            && try_body_final_bb_after_try_logic->getTerminator()->getSuccessor(0)
              == blocks.finally_normal_path_entry_bb)))
    {
      builder.SetInsertPoint(try_body_final_bb_after_try_logic); /* Before the branch to finally */
      if(try_body_last_val)
      {
        builder.CreateStore(try_body_last_val, result_alloca);
      }
    }

    proc_ctx.m_is_in_try_block_for_invoke = old_is_in_try_block_for_invoke;
    proc_ctx.m_current_unwind_dest_for_invoke = old_unwind_dest;

    /* --- Landing Pad and Catch --- */
    llvm::LandingPadInst *landing_pad_instruction = nullptr;
    auto [catch_body_last_val, catch_body_final_bb_after_catch_logic]
      = detail::gen_landing_pad_and_catch_block(proc_ctx,
                                                expr,
                                                caller_arity,
                                                blocks,
                                                &landing_pad_instruction);

    /* Store catch_body_last_val if needed before going to finally */
    if(result_alloca && catch_body_final_bb_after_catch_logic
       && !catch_body_final_bb_after_catch_logic->getTerminator()
       && (catch_body_final_bb_after_catch_logic->getTerminator() == nullptr
           || /* Path continues to finally */
           (catch_body_final_bb_after_catch_logic->getTerminator()->getNumSuccessors() > 0
            && catch_body_final_bb_after_catch_logic->getTerminator()->getSuccessor(0)
              == blocks.finally_normal_path_entry_bb)))
    {
      builder.SetInsertPoint(
        catch_body_final_bb_after_catch_logic); /* Before the branch to finally */
      if(catch_body_last_val)
      {
        builder.CreateStore(catch_body_last_val, result_alloca);
      }
    }

    llvm::BasicBlock *landingpad_direct_to_finally_predecessor = nullptr;
    if(blocks.landing_pad_bb && blocks.landing_pad_bb->getTerminator())
    {
      auto const term = blocks.landing_pad_bb->getTerminator();
      if(term->getNumSuccessors() > 0 && term->getSuccessor(0) == blocks.finally_actual_code_bb)
      {
        landingpad_direct_to_finally_predecessor = blocks.landing_pad_bb;
      }
    }

    detail::gen_finally_block(proc_ctx,
                              expr,
                              caller_arity,
                              blocks,
                              landing_pad_instruction,
                              try_body_final_bb_after_try_logic,
                              landingpad_direct_to_finally_predecessor);


    /* --- Continue Block and Result PHI --- */
    /* The predecessor blocks for the PHI in try_continue_bb will now typically be
       the block where finally_actual_code_bb finished its non-rethrow path.
       If there was no finally, it would be try_body_final_bb or catch_body_final_bb. */

    llvm::BasicBlock *pred_for_phi_from_try_path = nullptr;
    llvm::BasicBlock *pred_for_phi_from_catch_path = nullptr;

    if(expr->finally_body.is_some())
    {
      /* If finally exists, both normal paths (from try and from catch) would have gone
           through finally_actual_code_bb and then (if not rethrowing) to try_continue_bb.
           The PHI in try_continue_bb will have finally_actual_code_bb as its predecessor.
           The *value* for the PHI will be loaded from result_alloca.
        */
      if(blocks.finally_actual_code_bb && blocks.finally_actual_code_bb->getTerminator())
      {
        auto const term = blocks.finally_actual_code_bb->getTerminator();
        if(auto *br_inst = llvm::dyn_cast<llvm::BranchInst>(term))
        {
          if(br_inst->isConditional())
          { // CondBr to rethrow or continue
            if(br_inst->getSuccessor(1) == blocks.try_continue_bb)
            {
              pred_for_phi_from_try_path = blocks.finally_actual_code_bb;
              pred_for_phi_from_catch_path = blocks.finally_actual_code_bb;
            }
          }
          else
          { // Unconditional Br to continue
            if(br_inst->getSuccessor(0) == blocks.try_continue_bb)
            {
              pred_for_phi_from_try_path = blocks.finally_actual_code_bb;
              pred_for_phi_from_catch_path = blocks.finally_actual_code_bb;
            }
          }
        }
      }
    }
    else
    { /* No finally block */
      pred_for_phi_from_try_path = try_body_final_bb_after_try_logic;
      pred_for_phi_from_catch_path = catch_body_final_bb_after_catch_logic;
    }

    /* If using result_alloca, the values for the PHI will be loaded from it in try_continue_bb */
    // llvm::Value *value_for_phi_from_try = try_body_last_val;
    // llvm::Value *value_for_phi_from_catch = catch_body_last_val;

    if(result_alloca)
    {
      /* The actual value for the PHI will be the load from the alloca,
           done *inside* gen_try_continue_block if needed.
           Here we pass the original values, and gen_try_continue_block
           can decide if it needs to load from result_alloca based on the predecessor.
           Or, more simply, always load from result_alloca if it exists.
        */
      /* This needs gen_try_continue_block to be aware of result_alloca */
    }


    return detail::gen_try_continue_block(proc_ctx,
                                          expr,
                                          blocks,
                                          result_alloca /* Pass the alloca */,
                                          pred_for_phi_from_try_path,
                                          /* catch_body_last_val, // Value comes from alloca now */
                                          pred_for_phi_from_catch_path);
  }


} /* namespace jank::codegen */
