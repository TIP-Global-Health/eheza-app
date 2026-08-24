---
name: e2e-local-run-procedure
description: "How to run E-Heza e2e tests locally: switch the main tree to the branch, wait for gulp to finish compiling, then run"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: c7019686-e437-4d8b-9f2d-a2a4c507d45d
  modified: 2026-07-27T09:00:48.406Z
---

**When asked to run an e2e test, the procedure is: switch the main tree to the branch under test → make sure gulp has finished compiling → then run.** (User, 2026-07-27.)

**Why:** `playwright.config.ts` has `webServer: undefined`, so Playwright starts nothing. The app is served at `localhost:3000` by the `ddev gulp` watch running in the user's terminal, which builds **`/var/www/html/ihangane/client/serve/Main.js` from the MAIN tree only** — the scratchpad worktrees are not mounted in ddev. Running a test while the main tree sits on a different branch silently exercises the wrong build, and the test fails for reasons that have nothing to do with the change.

## How to apply

1. **Be on the branch in the main tree** — which since 2026-07-27 is where the work happens anyway, see `[[develop-in-the-main-tree]]`. If a branch is still held by an old worktree, detach it there first (`git checkout --detach`), since one branch cannot be checked out twice. Return to `develop` when the PR merges.
   ⚠ **Use `git -C <path>`, never `cd`** — a `cd` earlier in a compound command persists, and a "switch the main tree" that silently acted on the worktree (and then reported the worktree's branch back as the main tree's) cost two rounds on 2026-07-27.
2. **Wait for gulp to finish compiling.** Confirm the new code is actually being served, e.g. `curl -s localhost:3000/Main.js | grep -c '<a class or string the change adds>'`. A branch checkout does not always trigger the watcher immediately.
3. **Run from the main tree** (`/var/www/html/ihangane/client`): `drushEnv()` derives the ddev project from `process.cwd()` (`e2e/helpers/device.ts:16`), so running from a worktree makes ddev try to start a **second project** and fail on the port-3000 bind. `E2E_DDEV_PROJECT=/var/www/html/ihangane` overrides it if a worktree run is ever needed.
4. `RECORD=1` → headed + video (`headless: !recording`); video lands in `client/test-results/<test>/video.webm`. "Failed to convert" at the end is only the optional mp4/gif step — `ffmpeg` is not on PATH — not a test failure.

## Environment facts

- **`EHEZA_SITE` must be `rwanda`** (`.ddev/config.local.yaml`). The suite hard-codes Rwanda fixtures — village `Akanduga`, `Nyange Health Center`. On a Burundi install login times out on the village screen and **no e2e test in the repo can pass**.
- Playwright 1.58.2 needs `chromium-1208`; installed 2026-07-27 alongside the pre-existing 1228.
- Don't edit e2e helpers in the main tree by mistake — they belong to the branch's worktree.

Related: [[pre-push-code-review-gate]], [[elm-version-must-match-compiler-exactly]]
