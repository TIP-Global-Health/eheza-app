#!/bin/bash
set -euo pipefail

# ---------------------------------------------------------------------------- #
#
# Run the Behat/WebDriverIO tests.
#
# ---------------------------------------------------------------------------- #

# Simple Docker run to execute Behat.
docker run -e "BUILD_WEBDRIVERIO=0" -p 8080:80 server
exit $?
