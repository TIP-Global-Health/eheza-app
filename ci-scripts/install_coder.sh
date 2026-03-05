#!/bin/bash
set -e

# ---------------------------------------------------------------------------- #
#
# Installs The coder library so we can use t for code reviews.
#
# ---------------------------------------------------------------------------- #

# Packagist dropped Composer 1 support (Sep 2025). Download Composer 2 for
# global tool installs; the project's Composer 1 is not affected.
COMPOSER2="/tmp/composer2.phar"
if [[ ! -f "$COMPOSER2" ]]; then
  curl -sS https://getcomposer.org/download/latest-stable/composer.phar -o "$COMPOSER2"
fi

COMPOSER_MEMORY_LIMIT=-1 php "$COMPOSER2" global require squizlabs/php_codesniffer:3.5.6
COMPOSER_MEMORY_LIMIT=-1 php "$COMPOSER2" global require drupal/coder:8.3.9
if [[ -f ~/.composer/vendor/bin/phpcs ]]
then
  ~/.composer/vendor/bin/phpcs --config-set installed_paths "$HOME"/.composer/vendor/drupal/coder/coder_sniffer
else
  phpcs --config-set installed_paths "$HOME"/.config/composer/vendor/drupal/coder/coder_sniffer
fi
