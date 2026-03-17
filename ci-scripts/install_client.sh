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
sed -i 's/module LocalConfig.Example/module LocalConfig/' ./src/elm/LocalConfig.elm

# On CI, add swap space to prevent OOM kills during Elm compilation.
# The Elm compiler is memory-intensive and can exceed the 8GB available
# on CircleCI large machines when running alongside DDEV containers.
if [ -n "$CIRCLECI" ]; then
  echo "Adding 2GB swap space for Elm compilation..."
  sudo fallocate -l 2G /swapfile
  sudo chmod 600 /swapfile
  sudo mkswap /swapfile
  sudo swapon /swapfile
fi

if [ -z "$DEPLOY" ]
then
  gulp build
else
  gulp publish
fi
