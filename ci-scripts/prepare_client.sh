#!/bin/sh
set -e

# ---------------------------------------------------------------------------- #
#
# Prepare client.
#
# ---------------------------------------------------------------------------- #

"$TRAVIS_BUILD_DIR"/sysconfcpus/bin/sysconfcpus -n 2 elm-make --yes
cp ./client/src/elm/LocalConfig.Example.elm ./client/src/elm/LocalConfig.elm
