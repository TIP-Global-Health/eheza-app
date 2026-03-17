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
    docker ps -q --filter "label=com.ddev.site-name" | xargs -r docker stop || true
  fi

  gulp build

  if [ -n "$CIRCLECI" ]; then
    echo "Restarting DDEV containers..."
    docker ps -aq --filter "label=com.ddev.site-name" | xargs -r docker start || true
    # Wait for MariaDB to accept connections before proceeding.
    echo "Waiting for MariaDB to be ready..."
    ATTEMPTS=0
    while [ $ATTEMPTS -lt 30 ]; do
      if ddev drush status --fields=db-status 2>/dev/null | grep -q "Connected"; then
        echo "MariaDB is ready."
        break
      fi
      ATTEMPTS=$((ATTEMPTS + 1))
      sleep 2
    done
  fi
else
  gulp publish
fi
