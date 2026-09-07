---
name: stacked-pr-base-is-parent-branch
description: ⛔ A stacked PR's GitHub base must be the PARENT BRANCH, not develop — basing on develop inflates the diff with the parents' commits and lets an out-of-order merge drag them in
metadata:
  type: feedback
---

When a PR is built on top of another open PR, open it with `--base <parent-branch>`, not
`--base develop`. Naming the stacked base in the PR body instead is not enough.

**Why:** with `--base develop`, GitHub shows the union of every PR in the stack — B-320's PR
#2212 listed 6 commits and 9 files when only 1 commit and 6 files were under review, and three
of those files belonged to the parent PRs. The review agent had to fall back to
`git diff HEAD~1 HEAD` to find the real scope. Worse, merging that PR first would have pulled
its two parents into `develop` with it; the "merge those first" line in the body is prose, not
a constraint. Based on the parent branch, the diff is only the new commit, an out-of-order
merge is impossible, and **GitHub retargets the base to `develop` automatically when the parent
merges** — nothing to do later.

**How to apply:** `gh pr create --base <parent-branch>` when the work sits on an open PR. To fix
one already opened against `develop`: `gh api repos/TIP-Global-Health/eheza-app/pulls/<N> -X PATCH
-f base=<parent-branch>` — see [[gh-pr-edit-projectcards-workaround]], `gh pr edit` is broken here.
Checks are attached to the commit sha, so retargeting does not re-run CI. The runbook's default
`gh pr create --base develop` is for unstacked work only — see [[pr-first-review-workflow]] and
[[worktree-per-item-for-parallel-sessions]].
