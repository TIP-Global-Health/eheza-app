#!/bin/bash
# circleci_wait.sh

CMD="$*"

TIMEOUT=4300
INTERVAL=240
$CMD &
CMD_PID=$!

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
exit 1
