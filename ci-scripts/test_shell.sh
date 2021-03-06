#!/bin/sh

# ---------------------------------------------------------------------------- #
#
# Run the ShellCheck review.
#
# ---------------------------------------------------------------------------- #

HAS_ERRORS=0

code_review () {
  echo "${LWHITE}$1${RESTORE}"
  # The exclusions are to ignore errors related to including other shell scripts
  # and allowing "cd dir" without explicit error handling.
  if ! docker run -v "/home/circleci/project":/scripts koalaman/shellcheck:v0.4.6 -e SC1091,SC1090,SC2181,SC2164 /scripts/"$1"; then
    HAS_ERRORS=1
  fi
}

SCRIPTS=$(find ci-scripts server/scripts -name '*.sh')
for FILE in $SCRIPTS;  do
  code_review "$FILE"
done

exit $HAS_ERRORS
