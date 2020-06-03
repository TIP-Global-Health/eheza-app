#!/bin/bash

set -e

cd "$TRAVIS_BUILD_DIR" || exit 1

# Make Git operations possible.
cp deployment-robot-key ~/.ssh/id_rsa
chmod 600 ~/.ssh/id_rsa

# Make the site semi-installed.
ddev config global --instrumentation-opt-in=false
ddev start
export ROOT="$TRAVIS_BUILD_DIR/server"
cd "$ROOT" || exit 1
pwd
drupal_make
symlink_externals
composer_install
create_sites_default_files_directory

# Install Robo.li.
ddev composer install

# Authenticate with Terminus.
ddev terminus auth:login --machine-token="$TERMINUS_TOKEN"

GIT_HOST="***REMOVED***"

ssh-keyscan -p 2222 $GIT_HOST >> ~/.ssh/known_hosts

git clone ***REMOVED*** .pantheon

# Make the DDEV container aware of your ssh.
ddev auth ssh
