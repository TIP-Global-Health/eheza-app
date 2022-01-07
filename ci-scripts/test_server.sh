#!/bin/bash
set -euo pipefail

# ---------------------------------------------------------------------------- #
#
# Run the Behat/WebDriverIO tests.
#
# ---------------------------------------------------------------------------- #

# Simple Docker run to execute Behat.
mkdir -p /tmp/cache
docker run -v /tmp/cache:/tmp/travis-cache -e "BUILD_WEBDRIVERIO=0" -p 8080:80 server
exit $?
