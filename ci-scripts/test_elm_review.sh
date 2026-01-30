#!/bin/bash

# ---------------------------------------------------------------------------- #
#
# Run the Elm Review checks.
#
# ---------------------------------------------------------------------------- #

cd client || exit 1

echo "Running elm-review for client APP ..."

# Temporarily disable exit on error to capture elm-review's exit code
set +e
npx elm-review
EXIT_CODE=$?
set -e

if [ $EXIT_CODE -eq 0 ]; then
  echo "✓ Elm review for client APP passed"
else
  echo "✗ Elm review for client APP found issues"
  exit $EXIT_CODE
fi

cd ../server/elm || exit 1

echo "Running elm-review for server Elm APP ..."

# Temporarily disable exit on error to capture elm-review's exit code
set +e
npx elm-review
EXIT_CODE=$?
set -e

if [ $EXIT_CODE -eq 0 ]; then
  echo "✓ Elm review for server Elm APP passed"
else
  echo "✗ Elm review for server Elm APP found issues"
fi

exit $EXIT_CODE
