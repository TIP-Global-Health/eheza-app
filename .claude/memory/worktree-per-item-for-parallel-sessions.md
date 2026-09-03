---
name: worktree-per-item-for-parallel-sessions
description: "Main tree stays on develop; each item is worked in a worktree at <repo>-wt/<branch>, created to work and released after pushing — reinstated 2026-08-27, lifecycle corrected 2026-08-28"
metadata:
  node_type: memory
  type: feedback
---

**The main tree (`/var/www/html/ihangane`) stays on `develop` permanently. Each item is worked in
its own worktree at `/var/www/html/ihangane-wt/<branch>`, made by
`.claude/scripts/new-worktree.sh <branch>`.** (User, 2026-08-27.)

**A worktree lasts as long as the working session, not as long as the PR.** Create it when you are
about to work the branch; **release it once the work is pushed**. If more work is needed — review
findings, a rework — recreate it with the same script. (User, 2026-08-28: *"Whenever you want to work
on the branch, you recreate with `.claude/scripts/new-worktree.sh`, after you done working and push,
you release it. If more work needed, as a result of review for example, you recreate it again."*)

This **replaces `develop-in-the-main-tree`** (2026-07-27), which had retired the worktree rule and
said to check the issue branch out in the main tree. That rule is dead — deleted, not parked.

**Why:** the user wants several Claude Code sessions working different backlog items at once. A
single shared tree can only hold one branch, so main-tree working serialized every session. It also
broke two things silently: the backlog is read from whatever branch the main tree is on (a feature
branch serves a stale queue), and the `Stop` hook refuses to commit bookkeeping off `develop`.

## How to apply

1. `.claude/scripts/new-worktree.sh <branch>` — fetches `origin/develop`, prunes, **refuses a branch
   another session already holds**, creates the worktree, symlinks `node_modules`, `src/generated`
   and `LocalConfig.elm`, and makes `client/elm-stuff` a **real directory** (symlinking it makes
   every compile and test silently run against the main tree's sources).
2. `git worktree list` is the claim board — read it before starting an item.
3. `git worktree remove <path>` once the work is pushed — not only when the PR merges. Recreate on
   demand for the next round of work on that branch.
4. **Always `git -C <path>`, never a bare `cd`** across two trees — the `cd` persists and silently
   retargets everything after it. That mistake cost two rounds on 2026-07-27.

## What this does NOT solve — and it is the reason the rule was dropped in July

**`ddev gulp` still builds only the main tree's `client/`,** and the ddev project is a single
instance rooted there. So local e2e and manual QA still need the branch's code in the main tree.
Static verification — `elm make`, `elm-test`, `elm-review`, `phpcs`, `php -l` — runs fine in a
worktree and is what most items need (a worktree compiles all 548 modules, verified 2026-08-27).

For an item that genuinely needs the running app, **borrow the main tree deliberately**: ask first,
one session at a time, and return it to `develop` afterwards. The user chose 2026-08-27 not to add a
lock or claim file for this yet.

Bookkeeping races are handled on the git side only: the `Stop` hook takes a `flock` and rebases onto
whatever another session pushed first. Two sessions editing `HANDOFF.md` or `queue.md` at the same
time can still clobber each other in the working tree — re-read immediately before writing.

Related: [[e2e-local-run-procedure]], [[verify-by-running-not-reasoning]]
