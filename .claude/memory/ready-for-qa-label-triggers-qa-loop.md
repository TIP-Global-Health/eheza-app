---
name: ready-for-qa-label-triggers-qa-loop
description: "Adding the `Ready for QA` label to a PR starts an automated qa-tester run on another laptop — the label IS the QA run"
metadata:
  node_type: memory
  type: project
  originSessionId: c23e1c87-800e-452e-a9b9-1fff6e0ca941
  modified: 2026-11-02T11:08:06.338Z
---

Another laptop runs a `qa-tester` loop. When a PR gets the `Ready for QA` label, that loop picks it up and runs `qa-tester` on it (user, 2026-11-02, after PR #2457).

**Why:** the label is not just a status marker; it dispatches real QA work on a separate machine with its own running app.

**How to apply:**
- After review round 2, the "suggest a qa-tester run" and "move to QA?" questions are one decision: labelling = QA runs. Say so in that message, e.g. "labelling starts the QA loop's run".
- Don't offer to run `qa-tester` here as well after labelling, and don't take this station's running app for it — that duplicates the loop.
- Still label only on the user's approval ([[pr-first-review-workflow]]).
- A QA verdict may arrive on the PR from that loop; read it before reporting the PR's state.
