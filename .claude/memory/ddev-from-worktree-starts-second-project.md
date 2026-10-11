---
name: ddev-from-worktree-starts-second-project
description: "⛔ any `ddev` command run from a worktree starts a SECOND DDEV project named `eheza-app` (tracked config.yaml name); the real one is `ihangane` (gitignored config.local.yaml override)"
metadata:
  node_type: memory
  type: feedback
  originSessionId: 72dabdef-ded5-43f7-99cf-538fff348690
  modified: 2026-10-09T12:53:19.476Z
---

⛔ **Never run `ddev …` with a worktree as the working directory.** The main tree's project is
`ihangane` only because the gitignored `.ddev/config.local.yaml` sets `name: ihangane`. A worktree
has just the tracked `.ddev/config.yaml` (`name: eheza-app`), so `ddev exec` there auto-starts a
new `eheza-app` project: registry entry, containers, an empty DB volume, generated `.ddev/` files.
It failed only because port 3000 was taken by the real project's gulp.

**Why:** 2026-10-09, B-460 / PR #2321. A script did `cd <repo root> && ddev exec …` and was run
from the worktree. Clean-up needed `ddev delete eheza-app --omit-snapshot -y`; the classifier
blocks Claude from running it, so the user had to.

**How to apply:**
- Run ddev commands as `(cd /var/www/html/ihangane && ddev …)`. `ci-scripts/test_sync_incident_recreation.sh`
  takes `DDEV_DIR=/var/www/html/ihangane` for this.
- ⛔ Never "restore" with a plain `ddev start` in the main tree: its `post-start` hooks run
  `drush site-install` + migrations and **wipe the local DB**. Use `ddev start --skip-hooks`.
- Before assuming the shared app broke, check `docker ps` for `ddev-ihangane-*`. Another
  project's containers going up or down does not touch it.

Related: [[worktree-per-item-for-parallel-sessions]], [[e2e-local-run-procedure]]
