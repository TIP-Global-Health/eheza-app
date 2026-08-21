#!/bin/bash

# ---------------------------------------------------------------------------- #
#
# Run the Elm unit tests (elm-test).
#
# The test modules live next to the source as src/elm/**/Test.elm (there is no
# tests/ directory), so the glob is passed explicitly. src/generated is
# gitignored, so the ZScore WHO fixtures and the Version module are generated
# first via gulp.
#
# ---------------------------------------------------------------------------- #

set -e

# Install the Elm toolchain (same versions as ci-scripts/install_client.sh).
npm install -g elm@latest-0.19.2
npm install -g elm-test@0.19.2-0
npm install --global gulp-cli

cd "$CIRCLE_WORKING_DIRECTORY"/client

# Project dev-dependencies (gulp plugins, elm-test). The Playwright browser
# download is skipped via PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD in the job env.
npm install

# LocalConfig is gitignored but imported by the app modules; create it from
# the example, the same way install_client.sh does.
cp ./src/elm/LocalConfig.Example.elm ./src/elm/LocalConfig.elm
sed -i 's/module LocalConfig.Example/module LocalConfig/' ./src/elm/LocalConfig.elm

# Generate the files elm-test needs to compile (src/generated is gitignored):
# the ZScore WHO reference fixtures and the Version module.
gulp zscore
gulp version

elm-test "src/elm/**/Test.elm"
