#!/bin/bash

# ==============================================================================
# --- Configuration ---
# ==============================================================================

# The primary file to execute.
PRIMARY_FILE="test_try2.jank"

# The file to run for inspecting the Intermediate Representation (IR)
# when the primary command fails or times out.
IR_FILE="test_try2_ir.jank"

# The timeout duration in seconds for both commands.
TIMEOUT_DURATION=5

# ==============================================================================
# --- Platform Compatibility ---
# ==============================================================================

# Detect the correct timeout command ('gtimeout' on macOS, 'timeout' on Linux).
if command -v gtimeout >/dev/null 2>&1; then
  TIMEOUT_CMD="gtimeout"
  # The exit code for a timeout from GNU coreutils is 124.
  TIMEOUT_EXIT_CODE=124
elif command -v timeout >/dev/null 2>&1; then
  TIMEOUT_CMD="timeout"
  TIMEOUT_EXIT_CODE=124
else
  # No timeout command was found.
  TIMEOUT_CMD=""
  echo "---"
  echo "status: \"WARNING\""
  echo "message: \"'timeout' or 'gtimeout' command not found. The script will proceed without a timeout.\""
  echo "suggestion: \"On macOS, install with Homebrew: 'brew install coreutils'\""
  echo "---"
  echo "" # Newline for separation
fi

# ==============================================================================
# --- Helper Functions ---
# ==============================================================================

# This function runs the command to inspect the generated IR, applying its own timeout.
function inspect_ir() {
  echo "" # Newline for separation
  echo "---"
  echo "status: \"INSPECTING_IR\""
  echo "message: \"The generated LLVM IR is:\""
  echo "command: \"build/jank run ${IR_FILE}\""
  echo "---"

  if [ -n "$TIMEOUT_CMD" ]; then
    # Execute IR inspection with a timeout.
    ir_output=$($TIMEOUT_CMD ${TIMEOUT_DURATION}s build/jank run ${IR_FILE} 2>&1)
    ir_exit_code=$?

    if [ $ir_exit_code -eq 0 ]; then
      echo "---"
      echo "status: \"IR_INSPECTION_SUCCESS\""
      echo "message: \"IR inspection command completed successfully.\""
      echo "output: |"
      echo "${ir_output}" | sed 's/^/  /'
      echo "---"
    elif [ $ir_exit_code -eq $TIMEOUT_EXIT_CODE ]; then
      echo "---"
      echo "status: \"IR_INSPECTION_TIMEOUT\""
      echo "message: \"Generating LLVM IR taking too much time, there is likely a inifinite loop bug in the c++ code\""
      echo "---"
    else
      echo "---"
      echo "status: \"IR_INSPECTION_FAILURE\""
      echo "message: \"IR inspection command failed with exit code ${ir_exit_code}.\""
      echo "error_output: |"
      echo "${ir_output}" | sed 's/^/  /'
      echo "---"
    fi
  else
    # Execute IR inspection without a timeout if command is not available.
    ir_output=$(build/jank run ${IR_FILE} 2>&1)
    ir_exit_code=$?

    if [ $ir_exit_code -eq 0 ]; then
       echo "---"
       echo "status: \"IR_INSPECTION_SUCCESS\""
       echo "message: \"IR inspection command completed successfully.\""
       echo "output: |"
       echo "${ir_output}" | sed 's/^/  /'
       echo "---"
    else
       echo "---"
       echo "status: \"IR_INSPECTION_FAILURE\""
       echo "message: \"IR inspection command failed with exit code ${ir_exit_code}.\""
       echo "error_output: |"
       echo "${ir_output}" | sed 's/^/  /'
       echo "---"
    fi
  fi
}

# ==============================================================================
# --- Script Logic ---
# ==============================================================================

echo "---"
echo "status: \"RUNNING\""
if [ -n "$TIMEOUT_CMD" ]; then
    echo "message: \"Executing primary command with a ${TIMEOUT_DURATION}s timeout.\""
else
    echo "message: \"Executing primary command (no timeout command available).\""
fi
echo "command: \"build/jank run ${PRIMARY_FILE}\""
echo "---"

# --- Execute with Timeout (if available) ---
if [ -n "$TIMEOUT_CMD" ]; then
    command_output=$($TIMEOUT_CMD ${TIMEOUT_DURATION}s build/jank run ${PRIMARY_FILE} 2>&1)
    exit_code=$?

    if [ $exit_code -eq 0 ]; then
        # --- Success Case ---
        echo "---"
        echo "status: \"SUCCESS\""
        echo "message: \"Primary command completed successfully.\""
        echo "output: |"
        echo "${command_output}" | sed 's/^/  /' # Indent for readability
        echo "---"

    elif [ $exit_code -eq $TIMEOUT_EXIT_CODE ]; then
        # --- Timeout Case ---
        echo "---"
        echo "status: \"TIMEOUT\""
        echo "message: \"Command timed out after ${TIMEOUT_DURATION} seconds. There is possibly an infinite loop.\""
        echo "---"
        inspect_ir

    else
        # --- Failure Case ---
        echo "---"
        echo "status: \"FAILURE\""
        echo "message: \"Primary command failed with exit code ${exit_code}.\""
        echo "error_output: |"
        echo "${command_output}" | sed 's/^/  /'
        echo "---"
        inspect_ir
    fi
# --- Execute without Timeout ---
else
    command_output=$(build/jank run ${PRIMARY_FILE} 2>&1)
    exit_code=$?

    if [ $exit_code -eq 0 ]; then
        # --- Success Case (No Timeout) ---
        echo "---"
        echo "status: \"SUCCESS\""
        echo "message: \"Primary command completed successfully.\""
        echo "output: |"
        echo "${command_output}" | sed 's/^/  /'
        echo "---"
    else
        # --- Failure Case (No Timeout) ---
        echo "---"
        echo "status: \"FAILURE\""
        echo "message: \"Primary command failed with exit code ${exit_code}.\""
        echo "error_output: |"
        echo "${command_output}" | sed 's/^/  /'
        echo "---"
        inspect_ir
    fi
fi
