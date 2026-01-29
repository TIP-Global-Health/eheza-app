#!/bin/bash

# ---------------------------------------------------------------------------- #
#
# Run the Elm Review checks.
#
# ---------------------------------------------------------------------------- #

cd client

echo "Running elm-review..."
npx elm-review

EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
  echo "✓ Elm review passed"
else
  echo "✗ Elm review found issues"
fi

exit $EXIT_CODE
