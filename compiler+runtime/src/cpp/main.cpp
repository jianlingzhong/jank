#if 1
  #include <iostream>
  #include <filesystem>

  #include <boost/filesystem.hpp>
  #include <boost/algorithm/string.hpp>

  #include <llvm-c/Target.h>
  #include <llvm/Support/CommandLine.h>
  #include <llvm/Support/ManagedStatic.h>
  #include <llvm/Support/TargetSelect.h>

//#include <cling/Interpreter/Interpreter.h>
//#include <cling/Interpreter/Value.h>
//#include <clang/Frontend/CompilerInstance.h>
//#include <clang/Lex/Preprocessor.h>

  #include <readline/readline.h>
  #include <readline/history.h>

  #include <folly/FBString.h>

  #include <jank/util/mapped_file.hpp>
  #include <jank/read/lex.hpp>
  #include <jank/read/parse.hpp>
  #include <jank/runtime/context.hpp>
  #include <jank/analyze/processor.hpp>
  #include <jank/codegen/processor.hpp>
  #include <jank/evaluate.hpp>
  #include <jank/jit/processor.hpp>
  #include <jank/native_persistent_string.hpp>

namespace jank
{
  void run(util::cli::options const &opts)
  {
    using namespace jank;
    using namespace jank::runtime;

    {
      profile::timer timer{ "require clojure.core" };
      __rt_ctx->load_module("/clojure.core").expect_ok();
    }

    {
      profile::timer timer{ "eval user code" };
      std::cout << runtime::detail::to_string(__rt_ctx->eval_file(opts.target_file)) << std::endl;
    }

    //ankerl::nanobench::Config config;
    //config.mMinEpochIterations = 1000000;
    //config.mOut = &std::cout;
    //config.mWarmup = 10000;


    //ankerl::nanobench::Bench().config(config).run
    //(
    //  "thing",
    //  [&]
    //  {
    //    auto const ret();
    //    ankerl::nanobench::doNotOptimizeAway(ret);
    //  }
    //);
  }

  void run_main(util::cli::options const &opts)
  {
    using namespace jank;
    using namespace jank::runtime;

    {
      profile::timer timer{ "require clojure.core" };
      __rt_ctx->load_module("/clojure.core").expect_ok();
    }

    {
      profile::timer timer{ "eval user code" };
      __rt_ctx->load_module("/" + opts.target_module).expect_ok();

      auto const main_var(__rt_ctx->find_var(opts.target_module, "-main").unwrap_or(nullptr));
      if(main_var)
      {
        /* TODO: Handle the case when `-main` accepts no arg. */
        runtime::detail::native_transient_vector extra_args;
        for(auto const &s : opts.extra_opts)
        {
          extra_args.push_back(make_box<runtime::obj::persistent_string>(s));
        }
        runtime::apply_to(main_var->deref(),
                          make_box<runtime::obj::persistent_vector>(extra_args.persistent()));
      }
      else
      {
        throw std::runtime_error{ fmt::format("Could not find #'{}/-main function!",
                                              opts.target_module) };
      }
    }
  }

  void compile(util::cli::options const &opts)
  {
    using namespace jank;
    using namespace jank::runtime;

    //__rt_ctx.load_module("/clojure.core").expect_ok();
    __rt_ctx->compile_module(opts.target_ns).expect_ok();
  }

  void repl(util::cli::options const &opts)
  {
    using namespace jank;
    using namespace jank::runtime;

    /* TODO: REPL server. */
    if(opts.repl_server)
    {
      throw std::runtime_error{ "Not yet implemented: REPL server" };
    }

    {
      profile::timer timer{ "require clojure.core" };
      __rt_ctx->load_module("/clojure.core").expect_ok();
    }

    if(!opts.target_module.empty())
    {
      profile::timer timer{ "load main" };
      __rt_ctx->load_module("/" + opts.target_module).expect_ok();
      dynamic_call(__rt_ctx->in_ns_var->deref(), make_box<obj::symbol>(opts.target_module));
    }

    /* By default, RL will do tab completion for files. We disable that here. */
    rl_bind_key('\t', rl_insert);

    /* TODO: Completion. */
    /* TODO: Syntax highlighting. */
    /* TODO: Multi-line input. */
    while(auto const buf = readline("> "))
    {
      native_transient_string line{ buf };
      boost::trim(line);
      if(line.empty())
      {
        continue;
      }

      /* TODO: Persist history to disk, á la .lein-repl-history. */
      /* History is persisted for this session only. */
      add_history(line.data());
      try
      {
        auto const res(__rt_ctx->eval_string(line));
        fmt::println("");
        fmt::println("{}", runtime::detail::to_string(res));
        //rt_ctx.jit_prc.eval_string(line);
      }
      /* TODO: Unify error handling. JEEZE! */
      catch(std::exception const &e)
      {
        fmt::println("Exception: {}", e.what());
      }
      catch(jank::runtime::object_ptr const o)
      {
        fmt::println("Exception: {}", jank::runtime::detail::to_string(o));
      }
      catch(jank::native_persistent_string const &s)
      {
        fmt::println("Exception: {}", s);
      }
      catch(jank::read::error const &e)
      {
        fmt::println("Read error ({} - {}): {}", e.start, e.end, e.message);
      }
    }
  }
}

// NOLINTNEXTLINE(bugprone-exception-escape): This can only happen if we fail to report an error.
int main(int const argc, char const **argv)
try
{
  using namespace jank;
  using namespace jank::runtime;

  /* The GC needs to enabled even before arg parsing, since our native types,
   * like strings, use the GC for allocations. It can still be configured later. */
  GC_set_all_interior_pointers(1);
  GC_enable();

  llvm::llvm_shutdown_obj Y{};

  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmPrinters();
  llvm::InitializeAllAsmParsers();

  auto const parse_result(util::cli::parse(argc, argv));
  if(parse_result.is_err())
  {
    return parse_result.expect_err();
  }
  auto const &opts(parse_result.expect_ok());
  //llvm::cl::ParseCommandLineOptions(argc, argv);

  if(opts.gc_incremental)
  {
    GC_enable_incremental();
  }

  profile::configure(opts);
  profile::timer timer{ "main" };

  __rt_ctx = new(GC) runtime::context{ opts };

  switch(opts.command)
  {
    case util::cli::command::run:
      run(opts);
      break;
    case util::cli::command::compile:
      compile(opts);
      break;
    case util::cli::command::repl:
      repl(opts);
      break;
    case util::cli::command::run_main:
      run_main(opts);
      break;
  }
}
/* TODO: Unify error handling. JEEZE! */
catch(std::exception const &e)
{
  fmt::println("Exception: {}", e.what());
}
catch(jank::runtime::object_ptr const o)
{
  fmt::println("Exception: {}", jank::runtime::detail::to_string(o));
}
catch(jank::native_persistent_string const &s)
{
  fmt::println("Exception: {}", s);
}
catch(jank::read::error const &e)
{
  fmt::println("Read error ({} - {}): {}", e.start, e.end, e.message);
}
catch(...)
{
  fmt::println("Unknown exception thrown");
}
#endif

#if 0
//===--- tools/clang-repl/ClangRepl.cpp - clang-repl - the Clang REPL -----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file implements a REPL tool on top of clang.
//
//===----------------------------------------------------------------------===//

  #include "clang/Basic/Diagnostic.h"
  #include "clang/Frontend/CompilerInstance.h"
  #include "clang/Frontend/FrontendDiagnostic.h"
  #include "clang/Interpreter/CodeCompletion.h"
  #include "clang/Interpreter/Interpreter.h"
  #include "clang/Lex/Preprocessor.h"
  #include "clang/Sema/Sema.h"

  #include "llvm/ExecutionEngine/Orc/LLJIT.h"
  #include "llvm/LineEditor/LineEditor.h"
  #include "llvm/Support/CommandLine.h"
  #include "llvm/Support/ManagedStatic.h" // llvm_shutdown
  #include "llvm/Support/Signals.h"
  #include "llvm/Support/TargetSelect.h"
  #include <optional>

  // Disable LSan for this test.
  // FIXME: Re-enable once we can assume GCC 13.2 or higher.
  // https://llvm.org/github.com/llvm/llvm-project/issues/67586.
  #if LLVM_ADDRESS_SANITIZER_BUILD || LLVM_HWADDRESS_SANITIZER_BUILD
    #include <sanitizer/lsan_interface.h>

LLVM_ATTRIBUTE_USED int __lsan_is_turned_off()
{
  return 1;
}
  #endif

static llvm::cl::opt<bool> CudaEnabled("cuda", llvm::cl::Hidden);
static llvm::cl::opt<std::string> CudaPath("cuda-path", llvm::cl::Hidden);
static llvm::cl::opt<std::string> OffloadArch("offload-arch", llvm::cl::Hidden);

static llvm::cl::list<std::string>
  ClangArgs("Xcc",
            llvm::cl::desc("Argument to pass to the CompilerInvocation"),
            llvm::cl::CommaSeparated);
static llvm::cl::opt<bool> OptHostSupportsJit("host-supports-jit", llvm::cl::Hidden);
static llvm::cl::list<std::string> OptInputs(llvm::cl::Positional, llvm::cl::desc("[code to run]"));

static void LLVMErrorHandler(void *UserData, char const *Message, bool GenCrashDiag)
{
  auto &Diags = *static_cast<clang::DiagnosticsEngine *>(UserData);

  Diags.Report(clang::diag::err_fe_error_backend) << Message;

  // Run the interrupt handlers to make sure any special cleanups get done, in
  // particular that we remove files registered with RemoveFileOnSignal.
  llvm::sys::RunInterruptHandlers();

  // We cannot recover from llvm errors.  When reporting a fatal error, exit
  // with status 70 to generate crash diagnostics.  For BSD systems this is
  // defined as an internal software error. Otherwise, exit with status 1.

  exit(GenCrashDiag ? 70 : 1);
}

// If we are running with -verify a reported has to be returned as unsuccess.
// This is relevant especially for the test suite.
static int checkDiagErrors(clang::CompilerInstance const *CI, bool HasError)
{
  unsigned Errs = CI->getDiagnostics().getClient()->getNumErrors();
  if(CI->getDiagnosticOpts().VerifyDiagnostics)
  {
    // If there was an error that came from the verifier we must return 1 as
    // an exit code for the process. This will make the test fail as expected.
    clang::DiagnosticConsumer *Client = CI->getDiagnostics().getClient();
    Client->EndSourceFile();
    Errs = Client->getNumErrors();

    // The interpreter expects BeginSourceFile/EndSourceFiles to be balanced.
    Client->BeginSourceFile(CI->getLangOpts(), &CI->getPreprocessor());
  }
  return (Errs || HasError) ? EXIT_FAILURE : EXIT_SUCCESS;
}

struct ReplListCompleter
{
  clang::IncrementalCompilerBuilder &CB;
  clang::Interpreter &MainInterp;
  ReplListCompleter(clang::IncrementalCompilerBuilder &CB, clang::Interpreter &Interp)
    : CB(CB)
    , MainInterp(Interp){};

  std::vector<llvm::LineEditor::Completion> operator()(llvm::StringRef Buffer, size_t Pos) const;
  std::vector<llvm::LineEditor::Completion>
  operator()(llvm::StringRef Buffer, size_t Pos, llvm::Error &ErrRes) const;
};

std::vector<llvm::LineEditor::Completion>
ReplListCompleter::operator()(llvm::StringRef Buffer, size_t Pos) const
{
  auto Err = llvm::Error::success();
  auto res = (*this)(Buffer, Pos, Err);
  if(Err)
  {
    llvm::logAllUnhandledErrors(std::move(Err), llvm::errs(), "error: ");
  }
  return res;
}

std::vector<llvm::LineEditor::Completion>
ReplListCompleter::operator()(llvm::StringRef Buffer, size_t Pos, llvm::Error &ErrRes) const
{
  std::vector<llvm::LineEditor::Completion> Comps;
  std::vector<std::string> Results;

  auto CI = CB.CreateCpp();
  if(auto Err = CI.takeError())
  {
    ErrRes = std::move(Err);
    return {};
  }

  size_t Lines = std::count(Buffer.begin(), std::next(Buffer.begin(), Pos), '\n') + 1;
  auto Interp = clang::Interpreter::create(std::move(*CI));

  if(auto Err = Interp.takeError())
  {
    // log the error and returns an empty vector;
    ErrRes = std::move(Err);

    return {};
  }
  auto *MainCI = (*Interp)->getCompilerInstance();
  auto CC = clang::ReplCodeCompleter();
  CC.codeComplete(MainCI, Buffer, Lines, Pos + 1, MainInterp.getCompilerInstance(), Results);
  for(auto c : Results)
  {
    if(c.find(CC.Prefix) == 0)
    {
      Comps.push_back(llvm::LineEditor::Completion(c.substr(CC.Prefix.size()), c));
    }
  }
  return Comps;
}

llvm::ExitOnError ExitOnErr;

int main(int argc, char const **argv)
{
  ExitOnErr.setBanner("clang-repl: ");
  llvm::cl::ParseCommandLineOptions(argc, argv);

  llvm::llvm_shutdown_obj Y; // Call llvm_shutdown() on exit.

  std::vector<char const *> ClangArgv(ClangArgs.size());
  std::transform(ClangArgs.begin(),
                 ClangArgs.end(),
                 ClangArgv.begin(),
                 [](std::string const &s) -> char const * { return s.data(); });
  // Initialize all targets (required for device offloading)
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmPrinters();
  llvm::InitializeAllAsmParsers();

  if(OptHostSupportsJit)
  {
    auto J = llvm::orc::LLJITBuilder().create();
    if(J)
    {
      llvm::outs() << "true\n";
    }
    else
    {
      llvm::consumeError(J.takeError());
      llvm::outs() << "false\n";
    }
    return 0;
  }

  clang::IncrementalCompilerBuilder CB;
  CB.SetCompilerArgs(ClangArgv);

  std::unique_ptr<clang::CompilerInstance> DeviceCI;
  if(CudaEnabled)
  {
    if(!CudaPath.empty())
    {
      CB.SetCudaSDK(CudaPath);
    }

    if(OffloadArch.empty())
    {
      OffloadArch = "sm_35";
    }
    CB.SetOffloadArch(OffloadArch);

    DeviceCI = ExitOnErr(CB.CreateCudaDevice());
  }

  // FIXME: Investigate if we could use runToolOnCodeWithArgs from tooling. It
  // can replace the boilerplate code for creation of the compiler instance.
  std::unique_ptr<clang::CompilerInstance> CI;
  if(CudaEnabled)
  {
    CI = ExitOnErr(CB.CreateCudaHost());
  }
  else
  {
    CI = ExitOnErr(CB.CreateCpp());
  }

  // Set an error handler, so that any LLVM backend diagnostics go through our
  // error handler.
  llvm::install_fatal_error_handler(LLVMErrorHandler, static_cast<void *>(&CI->getDiagnostics()));

  // Load any requested plugins.
  CI->LoadRequestedPlugins();
  if(CudaEnabled)
  {
    DeviceCI->LoadRequestedPlugins();
  }

  std::unique_ptr<clang::Interpreter> Interp;

  if(CudaEnabled)
  {
    Interp = ExitOnErr(clang::Interpreter::createWithCUDA(std::move(CI), std::move(DeviceCI)));

    if(CudaPath.empty())
    {
      ExitOnErr(Interp->LoadDynamicLibrary("libcudart.so"));
    }
    else
    {
      auto CudaRuntimeLibPath = CudaPath + "/lib/libcudart.so";
      ExitOnErr(Interp->LoadDynamicLibrary(CudaRuntimeLibPath.c_str()));
    }
  }
  else
  {
    Interp = ExitOnErr(clang::Interpreter::create(std::move(CI)));
  }

  for(std::string const &input : OptInputs)
  {
    if(auto Err = Interp->ParseAndExecute(input))
    {
      llvm::logAllUnhandledErrors(std::move(Err), llvm::errs(), "error: ");
    }
  }

  bool HasError = false;

  if(OptInputs.empty())
  {
    llvm::LineEditor LE("clang-repl");
    std::string Input;
    LE.setListCompleter(ReplListCompleter(CB, *Interp));
    while(std::optional<std::string> Line = LE.readLine())
    {
      llvm::StringRef L = *Line;
      L = L.trim();
      if(L.ends_with("\\"))
      {
        // FIXME: Support #ifdef X \ ...
        Input += L.drop_back(1);
        LE.setPrompt("clang-repl...   ");
        continue;
      }

      Input += L;
      if(Input == R"(%quit)")
      {
        break;
      }
      if(Input == R"(%undo)")
      {
        if(auto Err = Interp->Undo())
        {
          llvm::logAllUnhandledErrors(std::move(Err), llvm::errs(), "error: ");
          HasError = true;
        }
      }
      else if(Input.rfind("%lib ", 0) == 0)
      {
        if(auto Err = Interp->LoadDynamicLibrary(Input.data() + 5))
        {
          llvm::logAllUnhandledErrors(std::move(Err), llvm::errs(), "error: ");
          HasError = true;
        }
      }
      else if(auto Err = Interp->ParseAndExecute(Input))
      {
        llvm::logAllUnhandledErrors(std::move(Err), llvm::errs(), "error: ");
        HasError = true;
      }

      Input = "";
      LE.setPrompt("clang-repl> ");
    }
  }

  // Our error handler depends on the Diagnostics object, which we're
  // potentially about to delete. Uninstall the handler now so that any
  // later errors use the default handling behavior instead.
  llvm::remove_fatal_error_handler();

  return checkDiagErrors(Interp->getCompilerInstance(), HasError);
}
#endif