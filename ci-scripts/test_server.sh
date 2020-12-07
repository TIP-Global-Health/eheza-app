#!/bin/bash
set -euo pipefail

# ---------------------------------------------------------------------------- #
#
# Run the Behat/WebDriverIO tests.
#
# ---------------------------------------------------------------------------- #

# Simple Docker run to execute Behat.
mkdir -p "$TRAVIS_BUILD_DIR"/travis-cache
docker run -v "$TRAVIS_BUILD_DIR"/travis-cache:/tmp/travis-cache -it -e "BUILD_WEBDRIVERIO=0" -p 8080:80 server
exit $?
