# Project Overview

This project, "jank," is a native dialect of Clojure that runs on LLVM. It is a C++ project that uses Clang and LLVM to provide both Just-In-Time (JIT) and Ahead-Of-Time (AOT) compilation for the jank language. The core of the jank language itself is implemented in jank, as seen in `src/jank/clojure/core.jank`.

The project is built using CMake and has a two-phase build process. The first phase builds a preliminary version of the jank compiler, which is then used to compile the jank core library. The final executable is then linked with the compiled core library.

## Building and Running

The project is built with CMake. The following are the general steps to build and run the project:

1.  **Configure:** `cmake -B build`
2.  **Build:** `cmake --build build`

The main executable is `jank` in the `build` directory.

### Commands

The `jank` executable has several subcommands:

*   `run <file>`: Execute a jank file.
*   `run-main <module>`: Execute the `-main` function in a jank module.
*   `compile-module <module>`: Compile a jank module to an object file.
*   `compile <module>`: Ahead-of-time compile a jank project.
*   `repl`: Start a Read-Eval-Print Loop for jank.
*   `cpp-repl`: Start a C++ REPL.
*   `check-health`: Check the environment for dependencies.

### Testing

To run the test suite, the `jank_test` option must be enabled during CMake configuration:

1.  **Configure for testing:** `cmake -B build -Djank_test=ON`
2.  **Build:** `cmake --build build`
3.  **Run tests:** `ctest --test-dir build`

## Development Conventions

The project uses C++20 and follows the coding style enforced by `.clang-format`. It also uses `.clang-tidy` for static analysis. All dependencies are managed through CMake's `FetchContent` or are included as submodules in the `third-party` directory.
