---
name: discovery-excludes-open-prs
description: "Backlog discovery rounds never touch, merge-simulate or certify open PRs — scope is develop's code; the user merges PRs when they see fit"
metadata:
  node_type: memory
  type: feedback
  originSessionId: dbce2cfe-a6be-4346-b884-7780b80ceab5
  modified: 2026-11-01T19:56:01.552Z
---

Discovery rounds audit `origin/develop` only. Never propose units that read, merge-simulate, test or certify open PRs (even QA-approved ones bound for the next release).

**Why:** user, 2026-11-01, rejecting an R47 plan built around nine QA-approved PRs: "The open PRs are not to be touched. I'll direct to merge them when I see fit. Backlog discovery is not about that."

**How to apply:** frame rounds from develop's code, the coverage map and local data copies. If a finding overlaps an open PR's item, note it against that item instead of recording a duplicate. Merged code is fair game (the R40/R42/R46 merged-surface pattern). Related: [[pr-first-review-workflow]].
