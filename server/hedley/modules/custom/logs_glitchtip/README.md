# Logs GlitchTip

Sends watchdog events to GlitchTip over the Sentry protocol, using the
`sentry/sdk` package pulled in by Composer Manager.

Events at or below the configured severity threshold (`WATCHDOG_ERROR` by
default) are collected during the request, deduplicated, and sent in a single
batch on shutdown. Uncaught exceptions are captured with their trace.

## Configuring the DSN

This repository is public and a GlitchTip DSN is an ingest key, so no DSN is
committed. Nothing is sent until one is configured; a fresh checkout, DDEV and
CI therefore stay silent.

The DSN is read from, in order of precedence:

1. The `logs_glitchtip_dsn` Drupal variable. Set it with
   `drush vset logs_glitchtip_dsn '<dsn>'`, from `$conf['logs_glitchtip_dsn']`
   in `settings.php`, or on the settings form at
   `/admin/config/services/logs-glitchtip-settings`.
2. The `GLITCHTIP_DSN` environment variable.

For a scripted install, put the DSN in `server/config.sh` (gitignored, copied
from `server/default.config.sh`); the install and reset scripts push it into
the Drupal variable.

### On Pantheon

Pantheon offers no way to set an environment variable for a Drupal 7 site, so
the DSN has to reach the Drupal variable instead. Two options, per Pantheon
site:

- **Drush, once per environment.** Simplest, and the value travels along when
  the database is cloned Dev ← Live:
  ```bash
  terminus remote:drush <site>.<env> -- vset logs_glitchtip_dsn '<dsn>'
  ```
- **`sites/default/settings.php` in the Pantheon repository** (private, and
  excluded from the deploy rsync, so it is never overwritten):
  ```php
  $conf['logs_glitchtip_dsn'] = '<dsn>';
  ```
  This wins over the database value, so the settings form then has no effect.

The environment name needs no configuration here: it is derived from
`PANTHEON_SITE_NAME` and `PANTHEON_ENVIRONMENT`, which keeps Dev, Test and Live
apart even though they share a cloned database.

The `sentry/sdk` package must be present in `sites/all/vendor` before a deploy
— Composer runs locally, not on Pantheon, and the deploy commits the vendor
directory. Without it the module silently reports nothing.

## Environment name

The environment reported to GlitchTip is taken from the
`logs_glitchtip_environment` variable, then the `GLITCHTIP_ENVIRONMENT`
environment variable, then `<pantheon site>.<pantheon environment>`, and
finally `local`.

The country servers are self-hosted and expose no vendor environment variable,
so they must set one of the first two. That is also what tells the module the
site is a real deployment rather than a developer machine — without it,
`logs_glitchtip_auto_disable_on_local` suppresses everything.
