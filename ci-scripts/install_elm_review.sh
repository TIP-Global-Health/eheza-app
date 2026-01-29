#!/bin/sh
set -e

# ---------------------------------------------------------------------------- #
#
# Install Elm Review dependencies.
#
# ---------------------------------------------------------------------------- #

# No installation needed - npx will automatically fetch elm-review when needed
# This avoids issues with installing all client dependencies which may have
# native compilation requirements not needed for elm-review

rm -f ./src/elm/ConfigDeploy.elm
rm -f ./src/elm/ZScore/Test.elm

echo "elm-review will be fetched by npx on first use"
