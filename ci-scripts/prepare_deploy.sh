#!/bin/bash

set -e

cd "$TRAVIS_BUILD_DIR" || exit 1
source server/scripts/helper-functions.sh

# Make Git operations possible.
cp deployment-robot-key ~/.ssh/id_rsa
chmod 600 ~/.ssh/id_rsa

# Make the server-side compiled.
# For the client-side, see .travis.yml.
cd server || exit 1
export ROOT="$TRAVIS_BUILD_DIR/server"
cd "$ROOT" || exit 1
ddev config global --instrumentation-opt-in=false
ddev start
drupal_make
cd ..
symlink_externals

cd server || exit 1

# Install Robo.li.
ddev . "cd .. && composer install"

# Authenticate with Terminus.
ddev . terminus auth:login --machine-token="$TERMINUS_TOKEN"

GIT_HOST="***REMOVED***"

ssh-keyscan -p 2222 $GIT_HOST >> ~/.ssh/known_hosts

git clone ***REMOVED*** .pantheon

# Make the DDEV container aware of your ssh.
ddev auth ssh

# Workaround for non-matching NPM/node version inside DDEV and Travis.
git checkout client/package-lock.json
