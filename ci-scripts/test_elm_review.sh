#!/bin/bash

# ---------------------------------------------------------------------------- #
#
# Run the Elm Review checks.
#
# ---------------------------------------------------------------------------- #

cd client || exit 1

echo "Running elm-review..."

# Temporarily disable exit on error to capture elm-review's exit code
set +e
npx elm-review
EXIT_CODE=$?
set -e

if [ $EXIT_CODE -eq 0 ]; then
  echo "✓ Elm review passed"
else
  echo "✗ Elm review found issues"
fi

exit $EXIT_CODE
