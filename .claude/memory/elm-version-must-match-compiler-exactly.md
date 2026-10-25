---
name: elm-version-must-match-compiler-exactly
description: "elm.json \"elm-version\" must equal the installed compiler exactly (both directions error), so Elm upgrades are lockstep; the version is 0.19.3 since PR #2323 (2026-10-11)"
metadata: 
  node_type: memory
  type: reference
  originSessionId: d3b65968-9dec-4acd-b66a-c9bd7185b7c6
  modified: 2026-10-11T10:35:48.607Z
---

For Elm **applications**, `elm.json`'s `"elm-version"` must match the installed compiler **exactly**. Both directions hard-fail with `-- ELM VERSION MISMATCH --`: a 0.19.2 binary refuses `"0.19.3"`, and the other way round. (Package `elm.json` uses a *range* like `0.19.0 <= v < 0.20.0` and is unaffected.)

## ⛔ STANDING RULE: 0.19.3 IS the version — run EVERY check with it

PR #2323 merged 2026-10-11 (user asked to align everything on it). This supersedes the user's 2026-07-26 rule "We should use Elm 0.19.2 … do all the check using 0.19.2". Never verify an Elm change with an older compiler, and never accept a verification someone else ran with one.

**Aligned 2026-10-11:** host-global `elm` 0.19.3 and `elm-test` 0.19.3-0 (`npm i -g elm@latest-0.19.3 elm-test@0.19.3-0`); DDEV image 0.19.3 / 0.19.3-0; project-local `client/node_modules/.bin/elm-test` 0.19.3-0. Bare `elm make`, `elm-test` and `elm-review` all work without `--compiler`. Full compile: 548 modules.

**The silent-rewrite hazard.** An older `elm` run in a checkout does not always just fail — a 0.19.1 binary once **rewrote `client/elm.json`'s pin** as an uncommitted change (2026-07-26). ⇒ After any elm/elm-test/elm-review run, `grep '"elm-version"' client/elm.json` must still say 0.19.3; check `git diff` for a stray `elm.json` hunk before committing. Sub-agents (`/code-review`) call bare `elm` in their worktree, so this applies to their worktrees too; they also copy `elm-stuff` instead of symlinking it.

## The lockstep cut (every bump, in one commit)

- 4× `elm.json`: `client/`, `client/review/`, `server/elm/`, `server/elm/review/`.
- Install sites: `.ddev/web-build/Dockerfile` (binary + `elm-test`), `ci-scripts/install_client.sh`, `ci-scripts/install_elm_review.sh` (`ELM_VERSION=`), `ci-scripts/test_elm.sh`, `.gitpod.yml`, **`.circleci/config.yml` (the sync-incident build step, added by #2321)**, `client/package.json` (`elm-test`) + regenerate `client/package-lock.json`.
- Rebuild the committed `server/hedley/modules/custom/hedley_general/js/elm-main.js`; CI byte-compares it (`ci-scripts/test_server_elm_bundle.sh`).
- `git grep -n 'elm@latest\|elm-version\|ELM_VERSION\|releases/download'` finds them all. ⚠ A new install site merged into `develop` while the bump PR is open is NOT a conflict and the PR's CI cannot see it — merge `develop` into the PR and re-grep before merging (caught on #2323).
- `elm-test` is pinned to the compiler line (it hardcodes `elm-version` into its generated project), so it moves with the compiler.
- The DDEV image has **one** `elm` for `client/` and `server/elm/`; a partial upgrade is impossible.
- After merge: reinstall host Elm, `ddev restart --skip-hooks` (plain restart reinstalls Drupal), then `ddev client-install`. `elm-stuff/<version>/` and `$ELM_HOME/<version>/` are version-namespaced, so no cache clearing.

## elm-review gotchas (both hit 2026-07-13, B-003)

- **Its incremental cache LIES after you edit an import line.** A warm run reported `NoUnused.Variables: Imported module 'Backend.Entities' is not used` in files that clearly use it. `rm -rf elm-stuff && mkdir elm-stuff`, re-run → no errors. ⇒ Never act on a NoUnused finding from a warm run; confirm cold first.
- **elm-review cannot run in the MAIN tree**: `Found several modules named 'Config'` (`src/elm/Config.elm` collides with `src/elm/Config.Deploy.elm`). Worktrees don't have Config.elm, so review works there.

Related: [[elm-0193-upgrade-assessment]], [[gizra-elm-form-fork]], [[elm-fulltest-needs-elm-make-main]]
