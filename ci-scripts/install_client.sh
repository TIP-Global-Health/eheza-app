#!/bin/bash
set -e

# ---------------------------------------------------------------------------- #
#
# Install client dependencies.
#
# ---------------------------------------------------------------------------- #

echo -e "Host github.com\n\tStrictHostKeyChecking no\n" >> ~/.ssh/config

# Install global packages.
npm install -g elm@latest-0.19.1
npm install -g elm-test@0.18.12
npm install --global gulp-cli

cd "$TRAVIS_BUILD_DIR"/client
npm install

# Gulp is responsible for creating the `src/generated` files.
cp ./src/elm/LocalConfig.Example.elm ./src/elm/LocalConfig.elm
gulp build
