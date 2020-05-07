#!/bin/bash

set -e

cd "$TRAVIS_BUILD_DIR" || exit 1
source "$TRAVIS_BUILD_DIR/server/scripts/helper-functions.sh"

phpenv config-rm xdebug.ini

# Make Git operations possible.
cp deployment-robot-key ~/.ssh/id_rsa
chmod 600 ~/.ssh/id_rsa

# Make Terminus available.
cd ~ || exit 1
export COMPOSER_MEMORY_LIMIT=-1
curl -s -S -O https://raw.githubusercontent.com/pantheon-systems/terminus-installer/master/builds/installer.phar && php installer.phar install

# Make Drush available.
cd ~/vendor/bin || exit 1
wget --quiet https://github.com/drush-ops/drush/releases/download/8.3.0/drush.phar
mv drush.phar drush
chmod +x drush

# Make the site semi-installed.
export ROOT="$TRAVIS_BUILD_DIR/server"
cd "$ROOT" || exit 1
pwd
drupal_make
symlink_externals
composer_install
create_sites_default_files_directory

# Install Robo.ii
cd "$TRAVIS_BUILD_DIR" || exit 1
composer install

# Start DDEV, deployment happens from inside the container.
ddev start

# Authenticate with Terminus
ddev terminus auth:login --machine-token="$TERMINUS_TOKEN"

cd "$TRAVIS_BUILD_DIR" || exit 1

GIT_HOST="***REMOVED***"

ssh-keyscan -p 2222 $GIT_HOST >> ~/.ssh/known_hosts

# Make the DDEV container aware of your ssh
ddev auth ssh
