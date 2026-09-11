---
name: run-phpcs-locally
description: How to run the Drupal/DrupalPractice phpcs lint (the CI gate) locally on eheza-app
metadata: 
  node_type: memory
  type: reference
  originSessionId: 3a0f2003-dc62-444e-8130-8870758fa415
---

The phpcs lint IS runnable locally — don't claim "can't run phpcs".

**Easiest: `ddev phpcs`** — a project custom command (`.ddev/commands/web/phpcs`) that runs the
exact CI lint: `ci-scripts/test_coder.sh` under both `Drupal` and `DrupalPractice` standards across
all custom modules, auto-installing `drupal/coder` in the container if missing. Exit 0 = passes.
(Note: it sweeps ALL modules and ignores any file argument; `bare phpcs` is not on the container's
`$PATH`, which is why `ddev exec "command -v phpcs"` returns nothing — use the `ddev phpcs` command,
discoverable via `ddev help`.)

**Host fallback (single file, faster):** `composer` + `php` 7.4 are on the host. Install via
`bash ci-scripts/install_coder.sh` (squizlabs/php_codesniffer:3.5.6 + drupal/coder:8.3.9). Gotcha:
composer's global dir is the XDG path `~/.config/composer` (not `~/.composer`), and the script's
final `--config-set` runs bare `phpcs` (not on PATH) so it fails — register manually:
```
PHPCS=~/.config/composer/vendor/bin/phpcs
php "$PHPCS" --config-set installed_paths ~/.config/composer/vendor/drupal/coder/coder_sniffer
php "$PHPCS" --standard=Drupal -p --extensions=php,module,inc,install,test,profile,theme,js,css <path>
php "$PHPCS" --standard=DrupalPractice -p --extensions=php,module,inc,install,test,profile,theme,js,css <path>
```
