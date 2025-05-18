#pragma once

#include <jank/codegen/llvm_processor.hpp>
#include <jank/analyze/expr/try.hpp>

namespace jank::codegen
{

  namespace detail
  {

    /* Structure to hold all the basic blocks for a try/catch/finally construct */
    struct TryGenBlocks
    {
      llvm::BasicBlock *try_body_entry_bb{};
      llvm::BasicBlock *landing_pad_bb{};
      llvm::BasicBlock *catch_body_entry_bb{};
      llvm::BasicBlock *finally_code_bb{};
      llvm::BasicBlock *rethrow_bb{};
      llvm::BasicBlock *try_continue_bb{};
    };

    /* Helper to generate either a call or an invoke.
     * This is the crucial function that needs to be used for any expression
     * *inside* a try block that might throw a Jank exception. */
    llvm::Value *
    gen_expr_potentially_throwing(llvm_processor &proc_ctx,
                                  analyze::expression_ref an_expr,
                                  analyze::expr::function_arity const &current_fn_arity,
                                  llvm::BasicBlock *normal_dest_if_invoke,
                                  llvm::BasicBlock *unwind_dest_if_invoke);

    TryGenBlocks create_try_catch_finally_blocks(llvm_processor &proc_ctx,
                                                 analyze::expr::try_ref expr,
                                                 llvm::Function *current_llvm_fn);

    std::pair<llvm::Value *, llvm::BasicBlock *>
    gen_try_block_body(llvm_processor &proc_ctx,
                       analyze::expr::try_ref expr,
                       analyze::expr::function_arity &caller_arity,
                       TryGenBlocks &blocks);

    std::pair<llvm::Value *, llvm::BasicBlock *>
    gen_landing_pad_and_catch_block(llvm_processor &proc_ctx,
                                    analyze::expr::try_ref expr,
                                    analyze::expr::function_arity &caller_arity,
                                    TryGenBlocks &blocks,
                                    llvm::LandingPadInst **out_landing_pad_inst);

    void gen_finally_block(
      llvm_processor &proc_ctx,
      analyze::expr::try_ref expr,
      analyze::expr::function_arity &caller_arity,
      TryGenBlocks &blocks,
      llvm::Value *landing_pad_inst_val, /* The result of CreateLandingPad (type {ptr, i32}) */
      llvm::BasicBlock *from_try_normal_exit_bb,
      llvm::BasicBlock *from_catch_normal_exit_bb,
      llvm::BasicBlock *
        from_landingpad_unhandled_exit_bb /* BB from landingpad if no catch or catch didn't handle */
    );

    llvm::Value *gen_try_continue_block(llvm_processor &proc_ctx,
                                        analyze::expr::try_ref expr,
                                        TryGenBlocks &blocks,
                                        llvm::Value *try_body_result_val,
                                        llvm::BasicBlock *try_body_final_bb,
                                        llvm::Value *catch_body_result_val,
                                        llvm::BasicBlock *catch_body_final_bb);


  } /* namespace detail */

  /*
   * Main function to generate LLVM IR for a try/catch/finally expression
   * using native LLVM exception handling.
   */
  llvm::Value *gen_native_try_catch_finally(llvm_processor &proc_ctx,
                                            analyze::expr::try_ref expr,
                                            analyze::expr::function_arity &caller_arity);

} /* namespace jank::codegen */
