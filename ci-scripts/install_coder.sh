#!/bin/bash
set -e

# ---------------------------------------------------------------------------- #
#
# Installs The coder library so we can use t for code reviews.
#
# ---------------------------------------------------------------------------- #

COMPOSER_MEMORY_LIMIT=-1 composer global require squizlabs/php_codesniffer:3.5.6
COMPOSER_MEMORY_LIMIT=-1 composer global require drupal/coder:8.3.9
if [[ -f ~/.composer/vendor/bin/phpcs ]]
then
  ~/.composer/vendor/bin/phpcs --config-set installed_paths "$HOME"/.composer/vendor/drupal/coder/coder_sniffer
else
  phpcs --config-set installed_paths "$HOME"/.config/composer/vendor/drupal/coder/coder_sniffer
fi
