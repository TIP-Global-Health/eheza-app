---
name: elm-version-must-match-compiler-exactly
description: "elm.json \"elm-version\" must equal the installed compiler exactly (both directions error), so Elm upgrades are lockstep"
metadata: 
  node_type: memory
  type: reference
  originSessionId: d3b65968-9dec-4acd-b66a-c9bd7185b7c6
  modified: 2026-07-26T06:24:09.070Z
---

For Elm **applications**, `elm.json`'s `"elm-version"` must match the installed compiler **exactly**. Both directions hard-fail with `-- ELM VERSION MISMATCH --`: a 0.19.1 binary refuses `"0.19.2"`, and a 0.19.2 binary refuses `"0.19.1"`. (Package `elm.json` uses a *range* like `0.19.0 <= v < 0.20.0` and is unaffected.)

Consequences for this repo, verified 2026-07-08 during the 0.19.2 upgrade (PR #1912):
- An Elm compiler bump is a **lockstep cut**: `client/elm.json`, `client/review/elm.json`, `server/elm/elm.json`, `server/elm/review/elm.json`, plus every install site (`.ddev/web-build/Dockerfile`, `ci-scripts/install_client.sh`, `ci-scripts/install_elm_review.sh`, `ci-scripts/test_elm.sh`, `.gitpod.yml`) must move in one commit.
- The DDEV image installs **one** `elm` binary shared by `client/` and `server/elm/`, so a partial upgrade of just one app is impossible.
- `elm-test` is pinned to the compiler line: it writes a hardcoded `'elm-version'` into the project it generates (`lib/Generate.js`), so e.g. `elm-test@0.19.1-revision6` can never drive a 0.19.2 compiler.
- After such a merge, every dev must reinstall Elm and run `ddev restart` to rebuild the web image.
- `elm-stuff/<version>/` and `$ELM_HOME/<version>/packages` are version-namespaced, so no cache clearing is needed.

## ⛔ STANDING RULE (user, 2026-07-26): 0.19.2 IS the version — run EVERY check with it

"We should use Elm 0.19.2. Update memory and do all the check using 0.19.2." Never verify an Elm change with the host-global 0.19.1, and never accept a verification someone else ran with it.

**The silent-rewrite hazard that produced this rule.** Running a **0.19.1** `elm` inside a worktree cut from develop does NOT just fail — it **rewrites `client/elm.json`'s `"elm-version"` from `0.19.2` to `0.19.1`** as an uncommitted working-tree change. That is a one-line diff which is easy to commit by accident, and it breaks the build for everyone (both directions hard-fail, above).

⇒ **After ANY elm/elm-test/elm-review run, confirm the pin: `grep '"elm-version"' client/elm.json` must still say 0.19.2.** Check the final `git diff` for a stray `elm.json` hunk before committing.

⚠ **Sub-agents did this to you** (pre-fix). The `/code-review` skill verifies in the worktree it is reviewing and calls **bare `elm`**, so while the global was 0.19.1 it downgraded the pin behind your back; it also **copies** `elm-stuff` (861M) and `src/generated` instead of symlinking, and leaves `node_modules` missing. Seen 2026-07-26 on `wt-1990`. The copy/symlink sloppiness remains — still re-check a sub-agent's worktree before trusting or committing from it.

## ✅ FIXED 2026-07-26: host-global `elm` IS NOW 0.19.2

`npm install -g elm@latest-0.19.2` — global is `elm@0.19.2-0`, `which elm` → `~/.nvm/versions/node/v21.2.0/bin/elm`, `elm --version` → 0.19.2. All four repo `elm.json` pins were already 0.19.2, so nothing in the project wanted 0.19.1.

**Consequences — the scratchpad-binary workaround below is now OBSOLETE:**
- **`--compiler` is no longer required.** Bare `elm make`, bare `./node_modules/.bin/elm-test`, and bare `elm-review` all work and **leave the pin at 0.19.2** (verified on `wt-1990`: 548 modules, 2891 tests, elm-review "no errors", worktree clean afterwards).
- The `elm192` scratchpad dir is redundant; passing `--compiler` still works if a script already does.
- ⚠ **Global `elm-test` is still `0.19.1-revision17`** and cannot drive a 0.19.2 compiler (it hardcodes an elm-version into the project it generates). **Always use the project-local `client/node_modules/.bin/elm-test` (0.19.2-0).** This is not a regression — bare global `elm-test` already failed against this repo's 0.19.2 pin. Upgrade it if it ever gets used directly.
- ⚠ Other projects on this host that pin 0.19.1 will now fail with the global binary; use a local/nvm-scoped 0.19.1 there if that comes up.

## Host toolchain gap (as of 2026-07-12) — READ BEFORE VERIFYING ANY ELM CHANGE

**PR #1912 is merged: `develop` now pins `"elm-version": "0.19.2"`.** But the host's *global* `elm` (`~/.nvm/.../bin/elm`) is still **0.19.1**, so it hard-fails on any worktree cut from develop. The global `elm-test` (and `client/node_modules/.bin/elm-test`) is already **0.19.2-0**, and global `elm-format` (0.8.8) is version-agnostic — the compiler is the only gap.

Fix without touching the user's global install — put 0.19.2 in the scratchpad and point the tools at it:
```bash
npm install --prefix <scratchpad>/elm192 elm@latest-0.19.2   # binary: <scratchpad>/elm192/node_modules/.bin/elm
<scratchpad>/elm192/node_modules/.bin/elm make src/elm/Main.elm --output=/dev/null
./node_modules/.bin/elm-test   --compiler <scratchpad>/elm192/node_modules/.bin/elm "src/elm/**/Test.elm"
./node_modules/.bin/elm-review --compiler <scratchpad>/elm192/node_modules/.bin/elm
```
`elm-test`/`elm-review` default to `elm` on PATH (0.19.1) → **always pass `--compiler`**. The old `npx elm-test@0.19.1-revision6` workaround from pre-#1912 sessions is now obsolete and will fail.

Test-suite baseline on develop after the upgrade: **2761 passing** (`elm-test "src/elm/**/Test.elm"`); full compile is **548 modules** (baseline 2800 on develop @8e19e166a, 2026-07-13; **2891 on develop @8b46ef1b2, 2026-07-26**, still 548 modules).

## elm-review gotchas (both hit 2026-07-13, B-003)

- **Its incremental cache LIES after you edit an import line.** Adding `exposing (Dict)` to `import AssocList as Dict` made a warm run report `NoUnused.Variables: Imported module 'Backend.Entities' is not used` in Backend/Update.elm + Backend/Utils.elm — both FALSE (those files use `PersonId` etc. everywhere). `rm -rf elm-stuff && mkdir elm-stuff`, re-run → **"I found no errors!"**. ⇒ Never act on a NoUnused finding from a warm run; confirm cold first (a bogus "unused import" deletion would break the build).
- **elm-review cannot run in the MAIN tree at all**: it aborts with a GLOBAL error, `Found several modules named 'Config'` (`src/elm/Config.elm` — local/generated — collides with `src/elm/Config.Deploy.elm`). Worktrees don't have Config.elm, so review works there. Don't waste time diffing "does this error pre-exist on develop?" in the main tree — it can't tell you.

Related: [[gizra-elm-form-fork]], [[elm-fulltest-needs-elm-make-main]]
