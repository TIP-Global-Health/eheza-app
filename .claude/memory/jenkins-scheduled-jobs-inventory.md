---
name: jenkins-scheduled-jobs-inventory
description: "The real Jenkins (ci.gizra.com) job inventory for all E-Heza sites, read from config.xml on 2026-10-10 — schedules, scripts, and what is NOT in the repo"
metadata:
  node_type: memory
  type: reference
  originSessionId: cea3b84a-2b0d-4b02-b4fa-f45be4798fb9
  modified: 2026-10-10T13:22:11.257Z
---

Read via the user's logged-in Chrome on 2026-10-10 (`https://ci.gizra.com/job/<name>/config.xml`;
the view and API return 401 without a browser session). Views: Ihangane, TIP-SOM, UVL, VHW.
All jobs are freestyle shells calling `terminus(.new) drush <site>.<env> -- scr …`; Slack `#sitehealth`
on failure; `concurrentBuild=false` everywhere.

| Job (per site) | Schedule | What |
|---|---|---|
| `<site>.<env>__advanced_queue__1` (live, demo, nutrition, som, training, vhw for ihangane; som.live/demo; uvl.live/demo; vhw.live/training) | `H/10` | `advancedqueue --all --timeout=600` (live) |
| `ihangane live - Overnight operations phase1` | `H 23` daily | super-user on → `delete-duplicates.php`, `delete-duplicate-measurements.php`, `populate-edd.php` → super-user off; then the 10 `completion-generate-*-data.php --exclude_set=1` |
| `… phase2` | after phase1 succeeds | `recalculate-large-datasets.php --scope=health-centers / provinces / global` |
| `ihangane live - Process WhatsApp` | `H/5` | `delete-public-files.php`, `send-messages.php`, `delete-processed.php` |
| `Ihangane live backups` | `H 2` daily | `terminus backup:create` |
| `Ihangane Recurring reports - LIVE` | `H 1 * * 4` weekly | 5 `hedley_admin/scripts/generate-*-report.php` → markdown email to `hedley_admin_report_email` |
| `ihangane multidev - Delete duplicates` | `H 23` | loops qa vhw som nutrition training demo |
| `ihangane Nutrition - Overnight operations` | `H 23` | phase1+phase2 in one, plus `hedley_ncda/scripts/recalculate-large-datasets.php` and `completion-recalculate-large-datasets.php` |
| `som.live / uvl.live / vhw live - Overnight operations` | `H 23` | the 10 completion scripts + `recalculate-large-datasets.php` (no scope flag) |
| `som.live / uvl.live / vhw live - Delete duplicates and trigger Backup` | `H 23` | dedup + EDD + `terminus backup:create` |
| `Data Pipeline` | **disabled**, `0 5` | clones live → `aos-backend` multidev, runs `hedley_migrate/data-pipeline/scripts/export-delta.php --since-vid` and loads SQL into an external Postgres `encounters` DB (34.32.23.190, mTLS, Jenkins creds) — script NOT in develop or any pushed branch |

Also seen: WhatsApp jobs for som/uvl live are **disabled** (and misnamed `*.ive`); Pantheon core is on
**D7ES 7.106 (2026-08-19)**, i.e. the Drupal 7 Extended Support stream, PHP 8.4-capable since 7.105.

**How to apply:** this is the authoritative list of scheduled work a backend replacement must
re-home (see [[backend-replacement-discovery-2026-10]]). Ask the user where `export-delta.php` lives.
