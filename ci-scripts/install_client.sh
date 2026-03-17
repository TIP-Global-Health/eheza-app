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

if [ -z "$DEPLOY" ]
then
  # On CI, stop DDEV containers during the memory-intensive Elm compilation
  # to avoid OOM kills, then restart them after without re-running hooks.
  if [ -n "$CIRCLECI" ]; then
    echo "Stopping DDEV containers to free memory for Elm compilation..."
    DDEV_CONTAINERS=$(docker ps -q --filter "label=com.ddev.site-name")
    if [ -n "$DDEV_CONTAINERS" ]; then
      docker stop "$DDEV_CONTAINERS" || true
    fi
  fi

  gulp build

  if [ -n "$CIRCLECI" ]; then
    echo "Restarting DDEV containers..."
    DDEV_STOPPED=$(docker ps -aq --filter "label=com.ddev.site-name")
    if [ -n "$DDEV_STOPPED" ]; then
      docker start "$DDEV_STOPPED" || true
    fi
    # Wait for MariaDB to be ready.
    sleep 5
  fi
else
  gulp publish
fi
