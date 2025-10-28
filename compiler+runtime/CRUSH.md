# CRUSH.md

This file provides a summary of conventions and commands for the jank codebase.

## Build Commands

- **Configure (Release):** `./bin/configure -GNinja -DCMAKE_BUILD_TYPE=Release -Djank_local_clang=on`
- **Configure (Debug with Tests):** `./bin/configure -GNinja -DCMAKE_BUILD_TYPE=Debug -Djank_test=on -Djank_local_clang=on`
- **Compile:** `./bin/compile`

## Testing

- **Run all tests:** `./bin/test`
- **Run a single test case:** `./build/jank-test <test_case_name>`
- **Continuously run tests:** `./bin/watch ./bin/test`
- **Run a single .jank test file:** `build/jank run <path_to_file.jank>`

## Linting and Formatting

- **Format C++ code:** `./bin/format` (uses `clang-format`)
- **Lint shell scripts:** `shellcheck <script_path>` or `find bin -type f -exec shellcheck -o all {} +`

## Code Style

- **Language:** C++20
- **Formatting:** Adhere to the `.clang-format` file in the root directory.
- **Naming Conventions:**
    - `snake_case` for functions and variables.
    - `PascalCase` for classes and structs.
- **Error Handling:** Use `jtl::result` for functions that can fail, and `try...catch` blocks for exceptions.
- **Memory Management:** The project uses the Boehm-Demers-Weiser garbage collector. Smart pointers are not typically used for objects managed by the GC.
