# Implementation runbook

Validated across PRs #1889–#2099. Every warning here was paid for once already.

## Per finding

**1. Worktree.** `git fetch origin develop` (develop advances mid-session), `git worktree prune`
(dead sessions leave prunable worktrees in wiped tmpfs scratchpads), then
`git worktree add <scratchpad>/wt-<id> -b <id>-slug origin/develop`.
⛔ Never switch the main tree — the user is working in it.

**2. Elm build inputs** (skip for PHP/JS-only). Symlink from the main tree's `client/`:
`node_modules`, `src/generated`, `src/elm/LocalConfig.elm`.
⚠ **elm-stuff: `mkdir` a fresh one — do NOT symlink.** elm-test writes a generated project whose
relative source-directories resolve *through* the symlink into the main tree, so it silently
compiles and tests the main tree's sources and every pass is vacuous. Cold compile ~1–2 min.

**3. Verify by change type.**

- **Elm** — `elm-format --yes` the touched files; `elm make src/elm/Main.elm --output=/dev/null`
  (floods stdout; grep for `Success! Compiled N modules`, N drifts upward);
  `./node_modules/.bin/elm-test "src/elm/**/Test.elm"` — the glob is required, and use the
  **project-local** binary (the host one is too new). `./node_modules/.bin/elm-review` if imports
  or exposing changed. Watch for top-level names colliding with let-bindings elsewhere in the file.
- **JS** — `node --check`. No unit framework; build a discrimination harness in the scratchpad
  driving the real file (`global.self = global`, eval the source, stub fetch/caches/ports; get the
  pre-fix file with `git show origin/develop:path`).
- **PHP** — `php -l` plus `phpcs --standard=Drupal` and again with `DrupalPractice`, **with the
  full CI extension list** (`--extensions=php,module,inc,install,test,profile,theme,css,info,txt,md`);
  bare phpcs silently skips `.module`.
- **Discrimination test** — required for subtle logic: prove the test FAILS against pre-fix code
  (stash or revert the fix), then restore. For mechanical one-liners, compile + suite is enough;
  don't build heavy fixtures to test a sign flip.

**4. Release the worktree the moment the PR is pushed.** `git worktree remove <path> --force` once
`git status --short` and `git log @{u}..` are both empty. The user checks branches out locally and
a held worktree makes their `git checkout` fail. Recreating one later costs a minute; holding one
costs them every time.

**5. Ship.** `gh issue create` (mechanism / impact / fix) → commit `Fixes #N` with the
`Co-Authored-By` trailer and **no `[ci skip]`** → `git push -u` → `gh pr create --base develop`.
Then hand the user `/code-review medium <branch>` — always. Watch CI in the background:
`--watch` exits after the lint/unit tier, before the gated e2e and simpletest jobs spawn, so
re-check afterwards.

⚠ `gh pr edit` is broken on this repo (Projects-classic GraphQL). Use
`gh api repos/TIP-Global-Health/eheza-app/pulls/N -X PATCH -f title="…"`.

**6. Reviews.** Verify every claim against the code before acting — review bots are sometimes
exactly right and sometimes overstated. Reply per thread with the fixing commit.

**7. Bookkeep immediately** (compaction-safe): entry status → `✅ IMPLEMENTED (issue #, PR #,
branch, base sha, what was verified)`, update the tier line, append side-findings as new minors.

## Traps that have cost a cycle

- **SimpleTest fixtures**: some bundles need their fields **at create**, not after — insert hooks
  read `field_person` / `field_adult` / `field_clinic` as they run, and a later `->set()` throws
  `EntityMetadataWrapperException`. Look for a `HedleyWebTestBase` helper (`createPMTCT()`,
  `createClinic()`, `createHealthCenter()`) before hand-rolling. CI reports this as
  "N passes, 0 fails, and 2 exceptions" — an exception count, not a failure count, so **grep the
  log for `exception`, not just `fail`**.
- **Editing files with near-identical loops**: `hedley_reports.module` has four, and one target
  string appears twelve times. Edit by line number with the target line asserted first.
- **CI flake**: "Too long with no output (exceeded 4m0s)" in *Install and build client* is the
  elm-compile watchdog. Confirm against the previous commit's statuses, then retrigger with an
  empty commit. Failure logs are readable without auth via the CircleCI v1.1 API — job number from
  the status URL, then the failed step's presigned `output_url`.
