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
npm install -g elm-test@0.19.1-revision6
npm install --global gulp-cli
npm install -g bower

cd "$CIRCLE_WORKING_DIRECTORY"/client
bower install
npm install

# Gulp is responsible for creating the `src/generated` files.
cp ./src/elm/LocalConfig.Example.elm ./src/elm/LocalConfig.elm

if [ -z "$DEPLOY" ]
then
  gulp build
else
  gulp publish
fi

elm make ./src/elm/Main.elm
