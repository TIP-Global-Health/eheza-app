---
name: pr-description-issue-link-first-line
description: "Every PR description's FIRST line must link its issue by number (e.g. `Fixes"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: ddb96d65-c452-4faa-a2b9-d632c66951c6
---

When opening a PR, the **first line of the PR description** must link the issue by number — use the `Fixes #<n>` form (matches the existing #1917 precedent), then a blank line, then the rest of the body.

**Why:** the user wants the PR↔issue link visible at the top of every PR at a glance. In this repo PRs target `develop` while the GitHub default branch is `main`, so `Fixes #N` does NOT auto-close the issue (that happens later in the release-issue-reconciliation sweep) — but the reference must still be there, on the first line, for human visibility and the later sweep. See [[release-issue-reconciliation]].

**How to apply:** put `Fixes #<issue>` as line 1 of the PR body (not only in the commit message). Rule set by user 2026-07-09 after noticing PR #1920 had `Fixes #1919` only in the commit, not the description. Backfilled #1920 (#1919) and #1922 (#1921) at that time; #1917 already complied.
