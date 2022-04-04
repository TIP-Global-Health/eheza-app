#!/bin/bash

# ---------------------------------------------------------------------------- #
#
# Run the Elm Format reviews.
#
# ---------------------------------------------------------------------------- #

<<<<<<< HEAD
source "$CIRCLE_WORKING_DIRECTORY"/server/travis.config.sh

=======
>>>>>>> origin/main
HAS_ERRORS=0

SCRIPTS=$(find client/src -name '*.elm')
for FILE in $SCRIPTS;  do
  echo "Validating $FILE"
  if ! elm-format --validate "$FILE" --elm-version=0.19; then
    HAS_ERRORS=1
  fi
done

exit $HAS_ERRORS
