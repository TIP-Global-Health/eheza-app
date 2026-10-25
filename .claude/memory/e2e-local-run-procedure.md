---
name: e2e-local-run-procedure
description: "How to run E-Heza e2e tests locally: borrow the main tree for the branch (ask first — sessions run in parallel), wait for gulp to compile, run, then give it back"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: c7019686-e437-4d8b-9f2d-a2a4c507d45d
  modified: 2026-07-27T09:00:48.406Z
---

**When asked to run an e2e test, the procedure is: get the branch under test into the main tree → make sure gulp has finished compiling → run → return the main tree to `develop`.** (User, 2026-07-27; the borrow-and-return framing dates from 2026-08-27, when the main tree stopped being where work happens.)

**Why:** `playwright.config.ts` has `webServer: undefined`, so Playwright starts nothing. The app is served at `localhost:3000` by the `ddev gulp` watch running in the user's terminal, which builds **`/var/www/html/ihangane/client/serve/Main.js` from the MAIN tree only** — the scratchpad worktrees are not mounted in ddev. Running a test while the main tree sits on a different branch silently exercises the wrong build, and the test fails for reasons that have nothing to do with the change.

## How to apply

1. **Get the branch into the main tree — deliberately, and put it back.** Since 2026-08-27 the main tree is parked on `develop` and work happens in per-item worktrees (see `[[worktree-per-item-for-parallel-sessions]]`), so an e2e run is now the one thing that borrows it. **Ask first — other sessions are running** — then detach the worktree holding the branch (`git -C <worktree> checkout --detach`, one branch cannot be checked out twice), check it out in the main tree, run, and **return the main tree to `develop` and re-attach the worktree when done**.
   ⚠ **Use `git -C <path>`, never `cd`** — a `cd` earlier in a compound command persists, and a "switch the main tree" that silently acted on the worktree (and then reported the worktree's branch back as the main tree's) cost two rounds on 2026-07-27.
2. **Wait for gulp to finish compiling.** Confirm the new code is actually being served, e.g. `curl -s localhost:3000/Main.js | grep -c '<a class or string the change adds>'`. A branch checkout does not always trigger the watcher immediately.
3. **Run from the main tree** (`/var/www/html/ihangane/client`): `drushEnv()` derives the ddev project from `process.cwd()` (`e2e/helpers/device.ts:16`), so running from a worktree makes ddev try to start a **second project** and fail on the port-3000 bind. `E2E_DDEV_PROJECT=/var/www/html/ihangane` overrides it if a worktree run is ever needed.
4. `RECORD=1` → headed + video (`headless: !recording`); video lands in `client/test-results/<test>/video.webm`. "Failed to convert" at the end is only the optional mp4/gif step — `ffmpeg` is not on PATH — not a test failure.

## Environment facts

- **`EHEZA_SITE` must be `rwanda`** (`.ddev/config.local.yaml`). The suite hard-codes Rwanda fixtures — village `Akanduga`, `Nyange Health Center`. On a Burundi install login times out on the village screen and **no e2e test in the repo can pass**.
- ⛔ **The suite needs the fixture DB a fresh `./install` creates** (PIN `1234` nurse, village `Akanduga`). On 2026-09-27 the local DB was a Rwanda production copy (5.1M nodes): PIN `1234` logged in as a real user, the app showed an empty village list, and `login()` timed out on `Nyange Health Center`. Check first: `ddev drush sql-query "SELECT COUNT(*) FROM node WHERE type='village' AND title LIKE 'Akanduga%'"` must be 1 (the fixture is titled "Akanduga at Mbirima"; an exact-title check reads 0 on a good install).
- **Reinstalling the fixture DB** (user, 2026-10-07): uncomment the post-start site-install hooks in `.ddev/config.local.yaml` and `ddev restart` (~10 min). ⚠ Keep the prose lines among them commented, or the YAML breaks at the first one. Only failure seen: `composer install -d sites/default/files/composer` (exit 4), harmless for e2e. gulp is not started by it — `ddev gulp` in the background, then stop it inside the container (`ddev exec "kill <pid>"`) when done.
- Playwright 1.58.2 needs `chromium-1208`; installed 2026-07-27 alongside the pre-existing 1228.
- Don't edit e2e helpers in the main tree by mistake — they belong to the branch's worktree.

Related: [[pre-push-code-review-gate]], [[elm-version-must-match-compiler-exactly]]
