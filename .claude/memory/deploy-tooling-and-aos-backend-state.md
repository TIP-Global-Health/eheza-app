---
name: deploy-tooling-and-aos-backend-state
description: "State of the E-Heza Pantheon deploy tooling (skills, ddev terminus-auth, RoboFile fra) and the half-finished aos-backend multidev deploy — for session continuity"
metadata: 
  node_type: memory
  type: project
  originSessionId: a701beb6-1897-4e83-82ea-7cc14d8d6294
---

Deploy-tooling work (2026-06-21 session). Where things live:

**PR #1827** (branch `1826-deploy-release-skill` → develop; `MERGEABLE/CLEAN`, **not yet merged** as of 2026-06-21) adds:
- Two Claude skills — **`deploy-multidev`** (deploy any branch to a Pantheon multidev/review env) and **`deploy-release`** (full `main`→Dev→Test→Live + GitHub release) — plus shared `_deploy-common/deploy-reference.md`. They're only loadable on this branch until #1827 merges to develop.
- Execution model: the driver runs **everything except the deploy command itself**. The user runs only `ddev robo deploy:pantheon[-sync]`, **tee'd** to `/tmp/deploy-<env>.log` so the driver reads the result (not pasted). `ddev auth ssh`, `ddev gulp publish`, `ddev terminus-auth`, and post-deploy are all driver-run.
- New ddev command `.ddev/commands/web/terminus-auth` → **`ddev terminus-auth`** (companion to `ddev auth ssh`): logs terminus in from `TERMINUS_MACHINE_TOKEN`, or `ddev terminus-auth <token>`.

**Two-auth model** (this was the bug): `ddev auth ssh` authenticates the **git push** to Pantheon; **terminus** (for the post-deploy `remote:drush` cc/updb/fra/uli) needs its own auth via `TERMINUS_MACHINE_TOKEN` / `ddev terminus-auth`. SSH-only → deploy pushes code then fails "You are not logged in".

**#1828 (MERGED to develop):** RoboFile `deployPantheonSync` now auto-runs `fra -y` + a `cc all` after `updb`, every env. Per-env stack: `cc all, cc all, updb -y, fra -y, cc all, uli`. So `fra` is no longer manual — **except the Pantheon dashboard GUI promotion path** (Test/Live tabs) doesn't run robo, so post-deploy must be done by hand there. The Gizra/ihangane Deployment wiki + the #1827 skills both document this.

**LOOSE END — `aos-backend` multidev deploy (rwanda/ihangane):** code was built + pushed to the `aos-backend` Pantheon env, but its post-deploy `cc all`/`updb -y`/`uli`/`fra -y` **never ran** (terminus wasn't authed). To finish: make a token at dashboard.pantheon.io/machine-token/create, `ddev terminus-auth <token>` (or set `TERMINUS_MACHINE_TOKEN` in `.ddev/config.local.yaml` + `ddev restart`), then `ddev exec terminus remote:drush ihangane.aos-backend -- cc all` (×2), `updb -y`, `uli`, `fra -y`. Verify at https://aos-backend-ihangane.pantheonsite.io. (Local config is rwanda/ihangane; only `server/.pantheon-ihangane` clone exists.)

See also [[request-copilot-review-via-api]], [[delete-branch-on-merge]].
