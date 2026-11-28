---
name: blind-test-author-pilot
description: Pilot started 2026-11-03 — logic fixes get tests from a blind agent (issue + criteria only, branched from a seam commit); measure whether review round 2 still finds anything the tests miss
metadata:
  type: project
---

Since 2026-11-03 every logic defect in the backlog loop is built as: issue with an approved
`## Expected behaviour` block → seam commit → `blind-test-author` agent writes failing tests from
the criteria alone → fix goes green → `.claude/scripts/fail-first.sh` proves they failed first.
Defined in the process-backlog runbook, steps 2a–2b.

**Why:** the user no longer reads review comments or diffs and wants certainty from independent
tests rather than from reviews run by the same model that wrote the code. Their approval of the
criteria is the one human gate kept.

**How to apply:** follow the runbook; never let the implementer change what a blind test asserts.
Record per item: criteria count, how many failed first, edits after the fix, and whether round 2
found anything beyond the tests. After about ten items, that last column decides whether round 2
is dropped. The injected-defect ("saboteur") step was designed but deliberately left aside for
now. See [[pr-first-review-workflow]].
