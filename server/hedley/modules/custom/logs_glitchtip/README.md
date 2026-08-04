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

## Environment name

The environment reported to GlitchTip is taken from the
`logs_glitchtip_environment` variable, then the `GLITCHTIP_ENVIRONMENT`
environment variable, then `<pantheon site>.<pantheon environment>`, and
finally `local`.

The country servers are self-hosted and expose no vendor environment variable,
so they must set one of the first two. That is also what tells the module the
site is a real deployment rather than a developer machine — without it,
`logs_glitchtip_auto_disable_on_local` suppresses everything.
