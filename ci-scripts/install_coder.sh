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

# Composer refuses to install a package under a security advisory, and one was
# published on 2026-08-06 against every php_codesniffer below 3.13.6 - which
# stopped this job installing anything at all, on every branch. The advisory is
# an OS command injection, and this runs a linter over our own source in CI,
# where that is not reachable, so it is allowed through here.
#
# Upgrading instead is a bigger piece of work than it looks: 3.13.6 needs a
# newer coder, which enables sniffs that did not exist in 3.5.6 and reports
# hundreds of findings across the custom modules. Worth doing on its own, not
# while it is blocking every branch.
COMPOSER_MEMORY_LIMIT=-1 php "$COMPOSER2" global require --no-security-blocking squizlabs/php_codesniffer:3.5.6
COMPOSER_MEMORY_LIMIT=-1 php "$COMPOSER2" global require --no-security-blocking drupal/coder:8.3.9
if [[ -f ~/.composer/vendor/bin/phpcs ]]
then
  ~/.composer/vendor/bin/phpcs --config-set installed_paths "$HOME"/.composer/vendor/drupal/coder/coder_sniffer
else
  phpcs --config-set installed_paths "$HOME"/.config/composer/vendor/drupal/coder/coder_sniffer
fi
