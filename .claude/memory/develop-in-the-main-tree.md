---
name: develop-in-the-main-tree
description: "Work on the issue branch in the main checkout, not a scratchpad worktree — supersedes the old worktree-per-finding rule"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: c7019686-e437-4d8b-9f2d-a2a4c507d45d
  modified: 2026-07-27T09:01:08.644Z
---

**Check out the issue branch in the main tree (`/var/www/html/ihangane`) and work there.** Approved by the user 2026-07-27, replacing the old "worktree per finding, never switch the main tree's branch" rule.

**Why the old rule was retired:**

- **gulp only builds the main tree.** `ddev gulp` watches `/var/www/html/ihangane/client` and writes `client/serve/Main.js`; the scratchpad worktrees are not mounted in ddev. So any e2e or manual check required moving the branch back to the main tree regardless — the worktree bought isolation that had to be undone on every test run.
- **The dance itself caused the bugs it was meant to avoid.** Detach-worktree → checkout-main → run → put back is easy to get wrong. On 2026-07-27 a persisted `cd` meant a "switch the main tree" actually re-attached the worktree, and success was then reported from the worktree's own branch; the user spotted it.
- What gets compiled now always matches what is being edited.

**How to work:**

1. `git -C /var/www/html/ihangane checkout -b <branch> origin/develop` (or check out an existing one).
2. Edit, build, test, commit, push there. No symlinking of `node_modules` / `elm-stuff` / `src/generated` / `LocalConfig.elm` — the main tree has them all.
3. Return with `git checkout develop` once the PR merges.
4. **Always `git -C <path>`, never a bare `cd`** when touching more than one tree in a command — the `cd` persists and silently retargets everything after it.

**Still true:** the user's uncommitted `CLAUDE.md` edit and the untracked `server/.pantheon-*` directories live in the main tree and carry across checkouts — leave them alone. Old worktrees from earlier sessions still exist under the scratchpads; remove them with `git worktree remove <path>` once their branches are merged.

Related: [[e2e-local-run-procedure]], [[pre-push-code-review-gate]]
