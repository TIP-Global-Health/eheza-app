---
name: spec-mismatch-is-fixed-regardless-of-size
description: ⛔ behaviour that contradicts a written requirement gets fixed however rare — never recommend closing a spec mismatch as an edge case
metadata:
  node_type: memory
  type: feedback
  originSessionId: d21b8c4a-1872-4a04-bf7e-a397ca29a374
  modified: 2026-11-16T10:32:30.647Z
---

When the code contradicts a written requirement (spec issue, clinician answer on the issue), it is
fixed, however small the population. Sizing still informs priority, never whether.

User, 2026-11-16, on B-599 (1 visit ever on ihangane, spec #139 contradicted): "If the behaviour
does not match the requirements, we should fix, no matter how edge this is."

**Why:** I recommended closing B-599 as an edge case by analogy with B-581. B-581 was closed for a
different reason: no requirement was contradicted, and a hospital-referred patient is assumed
treated there.

**How to apply:** when presenting an item, first say whether it contradicts a written requirement.
If it does, recommend implementing, and present size as urgency only. Close or park on size alone
only when the code contradicts no requirement. Related: [[read-the-issue-for-requirements]],
[[diagnosis-defects-are-tier-1]].
