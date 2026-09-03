---
name: delete-branch-on-merge
description: "Standing preference — delete a PR's head branch when merging it (delete-on-merge) on eheza-app"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: a701beb6-1897-4e83-82ea-7cc14d8d6294
---

When merging a PR on TIP-Global-Health/eheza-app, **delete its head branch as part of the merge** (delete-on-merge), unless told otherwise.

**Why:** The user adopted this on 2026-06-17 (after merging #1812/#1814/#1816). They want merged branches cleaned up rather than accumulating. This supersedes the earlier ad-hoc habit of leaving merged test branches around.

**How to apply:**
- Prefer `gh pr merge <N> --merge --delete-branch` so the head branch is removed automatically.
- If a PR was already merged without deleting, remove the remote head with `git push origin --delete <branch>` and verify with `git ls-remote --heads origin <branch>` (empty = gone). Drop any local copy with `git branch -D <branch>` (switch off it first).
- Gotcha: `gh api -X DELETE repos/<owner>/<repo>/git/refs/heads/<branch>` works, but do NOT pass curl's `-w`/`--silent` flags — `gh api` rejects `-w` and the delete silently no-ops (the HTTP code comes back blank). `git push origin --delete` is the reliable path.
- Only the merged PR's head branch — never delete a branch with other open PRs stacked on it. See [[code-review-improvement-ledger]] for the storage-pressure stack (#1818←#1820) where order/base matters.
