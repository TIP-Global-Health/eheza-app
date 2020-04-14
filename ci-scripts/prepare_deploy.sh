#!/bin/bash

set -e

cd "$TRAVIS_BUILD_DIR" || exit 1
export ROOT="$TRAVIS_BUILD_DIR/server"
source "$TRAVIS_BUILD_DIR/server/scripts/helper-functions.sh"

phpenv config-rm xdebug.ini

# Make Git operations possible.
cp deployment-robot-key ~/.ssh/id_rsa
chmod 600 ~/.ssh/id_rsa

# Make the site semi-installed.
cd server
drupal_make
symlink_externals
composer_install
create_sites_default_files_directory

# Make Terminus available.
cd ~ || exit 1
export COMPOSER_MEMORY_LIMIT=-1
curl -s -S -O https://raw.githubusercontent.com/pantheon-systems/terminus-installer/master/builds/installer.phar && php installer.phar install

# Authenticate with Terminus
terminus auth:login --machine-token="$TERMINUS_TOKEN"

cd "$TRAVIS_BUILD_DIR" || exit 1

GIT_HOST="***REMOVED***"

ssh-keyscan -p 2222 $GIT_HOST >> ~/.ssh/known_hosts
git clone ***REMOVED*** /tmp/pantheon-ihangane
