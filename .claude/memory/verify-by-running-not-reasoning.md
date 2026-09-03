---
name: verify-by-running-not-reasoning
description: "When a claim can be checked by running something, run it — and check the whole chain, not just the step I changed"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: c7019686-e437-4d8b-9f2d-a2a4c507d45d
  modified: 2026-07-27T10:37:45.190Z
---

When something can be verified by running it, run it. Reasoning about tooling behaviour
(process models, task ordering, when a framework re-reads a config) is where I get it wrong.

**Why:** on `e2e-preserve-recordings` I shipped two wrong fixes in a row. The first assumed a
Playwright config is read once per run — it is read again in every worker, so the videos went to
one directory and the teardown looked in another. The second moved the videos in `globalTeardown`,
which runs *before* `reporter.onEnd()`, so every path the reporter printed was already stale and
`--reporter=html` lost its videos. Both were caught by review, not by me, because my check stopped
at the step I had changed.

**How to apply:**
- Verify the *whole chain*, not the step I touched. "The file lands where I expect" is not the
  same as "everything downstream still finds it".
- Reach failure branches with a stub rather than skipping them — a fake `ffmpeg` on `PATH` that
  exits non-zero exercised the path a real run never produces.
- Discrimination-test: break the fix, confirm the test fails, restore. A test that cannot fail
  proves nothing.
- Prefer calling the function directly against staged directories over driving a browser — the
  seven cases in `client/e2e/recordings-kept.spec.ts` run in ~2s and cover states a passing run
  never reaches.
- Read the library's own source when behaviour is in question (`node_modules/@playwright/test/
  .../runner/tasks.js` settled both the worker-config and the teardown-ordering questions).

Related: [[e2e-local-run-procedure]], [[pre-push-code-review-gate]]
