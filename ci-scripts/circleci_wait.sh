#!/bin/bash
# circleci_wait.sh

# The command to execute
CMD="$*"

# Timeout duration (default to 20 minutes)
TIMEOUT=1200

# Interval in seconds between checks
INTERVAL=60

# Start the command in the background
$CMD &
CMD_PID=$!

# Wait for the command to finish or timeout
TIME_PASSED=0
while [ $TIME_PASSED -lt $TIMEOUT ]; do
    sleep $INTERVAL
    TIME_PASSED=$((TIME_PASSED + INTERVAL))
    echo "Still running... (elapsed time: ${TIME_PASSED}s)"

    # Check if the command is still active
    if ! kill -0 $CMD_PID 2> /dev/null; then
        echo "Command completed"
        wait $CMD_PID
        EXIT_STATUS=$?
        exit $EXIT_STATUS
    fi
done

echo "Timeout reached, terminating process"
kill $CMD_PID
