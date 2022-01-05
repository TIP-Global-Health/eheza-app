#!/bin/sh
set -e

# ---------------------------------------------------------------------------- #
#
# Installs The coder library so we can use t for code reviews.
#
# ---------------------------------------------------------------------------- #

cd "$CIRCLE_WORKING_DIRECTORY"
COMPOSER_MEMORY_LIMIT=-1 composer global require squizlabs/php_codesniffer:3.5.6
COMPOSER_MEMORY_LIMIT=-1 composer global require drupal/coder:8.3.9
phpcs --config-set installed_paths ~/.config/composer/vendor/drupal/coder/coder_sniffer
