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

**2a. Issue with acceptance criteria — before any code.** `gh issue create` now, not at ship
time: mechanism and impact as the entry gives them, no fix. End the body with an
`## Expected behaviour` block of five to ten lines, each a behaviour a tester could check, in the
app's own terms — no mechanism, no function names. Draft it from the issue trail and the entry,
show it to the user, and **wait for approval or edits**. This block is the contract the blind test
author and qa-tester read; approving it is the user's gate, in place of reading the diff.

    ## Expected behaviour
    - A pregnancy whose diagnosis is reconfirmed as `-continued` counts that diagnosis once.
    - Phase variants of one diagnosis collapse to one code.
    - A pregnancy with two different diagnoses counts two.

**2b. Seam commit, then blind tests.** Mandatory when the defect is in logic — a verdict, a
threshold, a meaning, a count. Skip it for mechanical one-liners, pure moves and config, and say so
in the bookkeeping record (step 7).

1. **Seam.** Extract or name the pure function the fix will live in, in the owning
   `Pages/<program>/…/Utils.elm` (never `Backend.Update` — a test importing it OOM-kills CI), with
   behaviour unchanged. Commit it alone as `Seam: <function>` and push the branch. Note its sha, S.
   Naming an existing function is a seam only while the fix keeps its signature. When the fix will
   remove an input (B-552 dropped today's date), no seam commit can keep the old behaviour under the
   new signature: S is develop, the author writes against today's signature, the gate runs before
   the fix, and call-site edits after it are expected — list them. Tell the author which input will
   disappear, so it carries that dimension in data the function keeps seeing (the encounter's end
   date, not a date argument), and record that the removed input is enforced by the type, not the
   tests.
2. **Blind author.** `.claude/scripts/new-worktree.sh <id>-tests S`, then start the
   `blind-test-author` agent (`Agent`, `subagent_type: blind-test-author`) with exactly this: the
   issue URL, the `## Expected behaviour` block pasted, the seam module and signatures, and the
   worktree path. Nothing else — not the entry, not the fix idea, not a diff.
3. **Build the fix meanwhile**, locally. Do not push it until the tests are merged, so there is
   nothing for the author to see.
4. **Merge.** When the author returns, `git merge origin/<id>-tests`. Read its table: every
   criterion row should be FAIL on S. A PASS row that is not a labelled guard means the criterion
   does not pin the defect — fix the criterion with the user and send the author that row again.
5. **Go green.** Edit a blind test only for a compile error against final signatures, and list each
   such edit in the bookkeeping record. Changing what a test asserts is not allowed: if one is
   wrong, the criterion is wrong, and that goes back to the user.
   ⛔ An input removed from the seam takes its tests' coverage with it. Either the input stays, or
   the dimension leaves the code too. Never move the decision it carried to a caller outside the
   seam — a gate relocated there is one no blind test reaches (B-548 round 1 did exactly this with
   the activity list, the shape of the original defect).
6. **Gate.** `.claude/scripts/fail-first.sh S` from the fix worktree. It runs this tree's test
   files against S in a throwaway worktree and passes only when they fail there.

**3. Verify by change type.**

- **Elm** — `elm-format --yes` the touched files; `elm make src/elm/Main.elm --output=/dev/null`
  (floods stdout; grep for `Success! Compiled N modules`, N drifts upward);
  `./node_modules/.bin/elm-test "src/elm/**/Test.elm"` — the glob is required, and use the
  **project-local** binary (the host one is too new). `./node_modules/.bin/elm-review` if imports
  or exposing changed. Watch for top-level names colliding with let-bindings elsewhere in the file.
  ⛔ Touched `server/elm/src/`? Its compiled bundle is committed: rebuild
  `server/hedley/modules/custom/hedley_general/js/elm-main.js` and commit it, then run
  `ci-scripts/test_server_elm_bundle.sh`. CI's `lint_elm_review` fails otherwise (PR #2301).
- **JS** — `node --check`. No unit framework; build a discrimination harness in the scratchpad
  driving the real file (`global.self = global`, eval the source, stub fetch/caches/ports; get the
  pre-fix file with `git show origin/develop:path`).
- **PHP** — `php -l` plus `phpcs --standard=Drupal` and again with `DrupalPractice`, **with the
  full CI extension list** (`--extensions=php,module,inc,install,test,profile,theme,css,info,txt,md`);
  bare phpcs silently skips `.module`.
- **Discrimination test** — required for subtle logic: prove the test FAILS against pre-fix code
  (stash or revert the fix), then restore. For a blind-tested item the gate in step 2b is that
  proof. For mechanical one-liners, compile + suite is enough;
  don't build heavy fixtures to test a sign flip.

**4. Remove the worktree once the work is pushed.** `git worktree remove <path>` once
`git status --short` and `git log @{u}..` are both empty. If review findings bring more work,
recreate it with `.claude/scripts/new-worktree.sh <branch>`; the branch lives on `origin`, not in
the directory.

**5. Ship.** Commit `Fixes #N` against the issue from step 2a, with the
`Co-Authored-By` trailer and **no `[ci skip]`** → `git push -u` → `gh pr create --base develop`.
Then run the medium review yourself — `Skill({skill: "code-review", args: "medium <branch>"})` — always,
in two rounds (step 6). Watch CI in the background:
`--watch` exits after the lint/unit tier, before the gated e2e and simpletest jobs spawn, so
re-check afterwards.

When the PR is merged, delete `<id>-tests` along with the fix branch: `git push origin --delete
<id>-tests`. The merge in step 2b already put its commits in `develop`.

⚠ `gh pr edit` is broken on this repo (Projects-classic GraphQL). Use
`gh api repos/TIP-Global-Health/eheza-app/pulls/N -X PATCH -f title="…"`.

**6. Reviews.** Two medium rounds. Round 1: fix or decline as you judge, push; nothing on the PR.
Round 2, on the pushed head: post inline, fix or decline as you judge, push, then present the
analysis — no approval wait. Ask first only for a fix that changes intended behaviour, needs a
clinician, or grows the diff beyond the problem.
Verify every claim against the code before acting — review bots are sometimes
exactly right and sometimes overstated. In the round-2 analysis also answer two fixed questions
yourself: does every `## Expected behaviour` line have a test, and was any blind test edited after
the fix. Note whether round 2 found anything the blind tests missed — that is the measure deciding
whether round 2 stays. Reply per thread with the fixing commit or the reason it
was declined, then resolve the thread (mutation in step 7).

**6a. Suggest QA.** Once round 2 is addressed, say whether a `qa-tester` run is worth it, and why:
does the change show on screen or in stored data, and would the e2e suite catch it? Suggest only;
the user decides, and the running app is shared.

In the same message, ask whether to move the PR to QA. Only when the user approves, add the label:
`gh api repos/TIP-Global-Health/eheza-app/issues/<N>/labels -f 'labels[]=Ready for QA'`

**7. Bookkeep immediately** (compaction-safe): entry status → `✅ IMPLEMENTED (issue #, PR #,
branch, base sha, what was verified, blind tests: <criteria>/<failed first>, edits after fix:
none|<list>, round 2 beyond the tests: none|<what>)` — or `blind tests: skipped (<why>)`, update the tier line, append side-findings as new minors,
run `reindex.py`, **then commit and push those files on `develop`** with `[ci skip]` — staging the
paths explicitly, never `git add -A` (`server/.pantheon-*` are untracked and not gitignored). A
record that exists only in the working tree is not a record.
⛔ **And close the loop on the PR: a push that answers a review finding is not finished until that
finding's thread says so.** Not "eventually" — in the same step that pushes the fix. A thread left
at *awaiting decision* after the work shipped contradicts the code, and is the same failure as
never posting it.
⛔ **Saying so means a reply AND a resolved thread.** A reply alone leaves the thread open on GitHub.
`gh` has no command for it; use the GraphQL mutation, with the id from `reviewThreads { nodes { id } }`:
`gh api graphql -f query='mutation($id:ID!){resolveReviewThread(input:{threadId:$id}){thread{isResolved}}}' -f id=<thread id>`
📌 A `Stop` hook (`.claude/hooks/commit-bookkeeping.sh`) commits and pushes these paths as a net,
and warns instead when the tree is not on `develop`. It is a backstop for a miss, not the plan —
its generated message says far less than one written here.

## Traps that have cost a cycle

- **Review rounds can undo the seam.** A round-1 fix that reintroduces an input the fix removed
  belongs inside the seam function, so the blind tests see it; placed in the caller it is
  unpinned, and the activity-dimension tests silently become same-activity twins (B-548).

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
