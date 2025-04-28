#!/usr/bin/env bash

# Tell cargo-minimize to expect standard Cargo output format
echo "minimize-fmt-cargo"

# --- Configuration ---
# Choose a unique string that ONLY appears when your specific linker error occurs.
# Good candidates from your previous output:
#   "Error: java.lang.ArrayIndexOutOfBoundsException"
#   "--- R8 FAILED ---"
#   "Error creating JAR file: R8 optimization failed"
# Let's use the most specific one related to the R8 crash:
#EXPECTED_ERROR_SUBSTRING="ArrayIndexOutOfBoundsException"
# Or use:
EXPECTED_ERROR_SUBSTRING="linking with"

# --- Execution ---
# Create a temporary file to capture stderr + stdout
# Using mktemp is safer than fixed filenames
OUTPUT_FILE=$(mktemp)

# Ensure the temporary file is removed when the script exits, regardless of success/failure
trap 'rm -f "$OUTPUT_FILE"' EXIT

echo "--- Running build command, capturing output to $OUTPUT_FILE ---"
# Run the build, redirecting both stdout and stderr (2>&1) to the temp file
cargo build --release > "$OUTPUT_FILE" 2>&1
EXIT_CODE=$?
echo "--- Build command finished with exit code: $EXIT_CODE ---"

REPRODUCED=1 # Default to 1 (failure / not reproduced)

# Check if the build command failed (non-zero exit code, likely 101 for cargo)
if [ $EXIT_CODE != 0 ]; then
    echo "Build command failed (code $EXIT_CODE). Checking output for specific error string..."
    # Search (-q quiet mode) for the specific error string within the captured output
    if grep -q "$EXPECTED_ERROR_SUBSTRING" "$OUTPUT_FILE"; then
        echo "Specific error substring \"$EXPECTED_ERROR_SUBSTRING\" found in output. Error Reproduced."
        REPRODUCED=0 # Set to 0 (success / reproduced)
    else
        echo "Build failed, but the specific error substring \"$EXPECTED_ERROR_SUBSTRING\" was *NOT* found."
        echo "--- Build Output (showing why it didn't match) ---"
        cat "$OUTPUT_FILE"
        echo "--- End Build Output ---"
        # Keep REPRODUCED=1 (not the error we are looking for)
    fi
else
    echo "Build command succeeded (code 0). Error Not Reproduced."
    # Keep REPRODUCED=1
fi

# Explicitly remove the temp file (trap should also cover this)
rm -f "$OUTPUT_FILE"

echo "--- Script exiting with code $REPRODUCED (0=reproduced, 1=not reproduced) ---"
exit $REPRODUCED