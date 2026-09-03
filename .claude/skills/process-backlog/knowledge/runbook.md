# Implementation runbook

Validated across PRs #1889–#2099. Every warning here was paid for once already.

## Per finding

**1. Worktree.** `.claude/scripts/new-worktree.sh <id>-slug` — it fetches `origin/develop`, prunes,
refuses a branch another session already holds, creates the worktree and does step 2's symlinks.
By hand: `git worktree add /var/www/html/ihangane-wt/<id>-slug -b <id>-slug origin/develop`.

Worktrees are **durable and shared**. They live in `/var/www/html/ihangane-wt/`, not in the session
scratchpad, so they outlive the session that made them and every other session sees them with
`git worktree list`. About 30 MB each — the heavy directories are symlinked in step 2.

⛔ **Never switch the main tree.** `/var/www/html/ihangane` is parked on `develop` permanently. It
is the backlog source of truth, the symlink donor for step 2, and the one tree ddev and gulp build.
A feature branch checked out there serves a stale queue and stops the `Stop` hook committing.

⛔ **Read `git worktree list` before claiming an item** — sessions run in parallel and another one
may already hold it. `git worktree prune` clears entries whose directory was removed by hand.

**2. Elm build inputs** — done for you by the script in step 1; by hand, symlink from the main
tree's `client/`: `node_modules`, `src/generated`, `src/elm/LocalConfig.elm`.
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

**4. Remove the worktree when the PR merges** — not when it is pushed. `git worktree remove <path>`
once `git status --short` and `git log @{u}..` are both empty. Holding it while the PR is open is
correct: review findings land against that branch, and nothing else needs the directory now that the
main tree stays on `develop`.

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
branch, base sha, what was verified)`, update the tier line, append side-findings as new minors,
run `reindex.py`, **then commit and push those files on `develop`** with `[ci skip]` — staging the
paths explicitly, never `git add -A` (`server/.pantheon-*` are untracked and not gitignored). A
record that exists only in the working tree is not a record.
⛔ **And close the loop on the PR: a push that answers a review finding is not finished until that
finding's thread says so.** Not "eventually" — in the same step that pushes the fix. A thread left
at *awaiting decision* after the work shipped contradicts the code, and is the same failure as
never posting it.
📌 A `Stop` hook (`.claude/hooks/commit-bookkeeping.sh`) commits and pushes these paths as a net,
and warns instead when the tree is not on `develop`. It is a backstop for a miss, not the plan —
its generated message says far less than one written here.

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
