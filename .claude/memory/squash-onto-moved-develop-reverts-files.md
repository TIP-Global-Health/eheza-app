---
name: squash-onto-moved-develop-reverts-files
description: git reset --soft onto a moved origin/develop silently pulls stale copies of unrelated files into the squashed commit
metadata:
  type: feedback
---

Squashing a worktree branch with `git reset --soft origin/develop && git commit` after `develop`
has moved pulls the branch's **stale copies of every file `develop` changed since the branch was
cut** into the commit — as a revert of that newer work.

**Why:** the soft reset moves HEAD to the new base but keeps the index holding the old tree, so
everything `develop` advanced past shows up as a change. It bit PR #2158 on 2026-08-27: a branch cut
before the backlog was updated silently reverted `HANDOFF.md` and `items/B-195.md`. Caught by the
code review, not by me.

**How to apply:** this workflow squashes often and every item lives in its own worktree while the
bookkeeping keeps landing on `develop`, so the window is always open. `git fetch` and rebase onto
the current base before squashing, and **read `git show --stat` after every squash** — the file
count is the tell. See [[worktree-per-item-for-parallel-sessions]] and
[[local-verification-vs-ci]].
