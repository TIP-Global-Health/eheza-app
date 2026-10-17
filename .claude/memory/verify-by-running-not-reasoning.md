---
name: verify-by-running-not-reasoning
description: "When a claim can be checked by running something, run it — and check the whole chain, not just the step I changed"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: c7019686-e437-4d8b-9f2d-a2a4c507d45d
  modified: 2026-09-07
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

- ⛔ **"It self-corrects on the next save" is not a safety argument until I check what the window
  lets a user WRITE.** On B-323 (PR #2210) I noticed a new diagnosis firing from a pending-input
  marker, traced that the next save recomputes and withdraws it, and shipped it as harmless. Review
  found the window is reachable and lets the nurse distribute PrEP from the wrong diagnosis — the
  diagnosis withdraws, the recorded prescription does not. A transient wrong state is only harmless
  if nothing durable can be written while it holds; enumerate what the UI offers during the window.
- ⛔ **After fixing one reader of a value, fix the LIST of readers.** On B-323 I widened an
  execution-note test in four places, then guarded a sentinel in only one of the three that read
  the same partner signs — the progress report went on asserting "Partner NOT taking ARVs" from a
  marker meaning "not answered yet". The grep that found the defect is the grep that finds the fix
  sites; run it again after settling the fix, not only before.

- ⛔⛔ **A PASSING e2e proves nothing until I have seen it FAIL on the pre-fix build.** On B-336
  (PR #2214) my new test was green for four consecutive runs against a bundle I had verified was
  pre-fix — it was passing for a reason unrelated to the fix. The generic `completeLabResults`
  helper takes the *first* real option of every result dropdown, i.e. **Positive**, so syphilis was
  diagnosed too, and `syphilisTreatmentCompleted` — a separate conjunct of `nextStepsTaskCompleted`
  — held the activity pending whatever the medication rule said. My steering had missed it because
  the tab is labelled **"Syphilis - RPR"** and I matched the label exactly. Run the discrimination
  first, and when a test passes where it should fail, stop reasoning and get evidence: the page
  snapshot in `test-results/*/error-context.md`, the `.tasks-count` text, the actual DB row
  (`field_prescribed_medication` held `none-recurrent`, confirming my model of the *write* was
  right and my model of the *readers* was wrong).
- ⛔ **A one-sided `toBeVisible` after clicking a tab is not an assertion.** A tab that failed to
  switch leaves the other tab's cards on screen and the check passes either way. Assert present in
  the expected tab AND absent from the other (`expectActivityInTab` in `client/e2e/helpers/common.ts`).
- ⭐ In E-Heza, the assertion that actually catches "the encounter closed without doing X" is that
  the app did **not** navigate to the progress report: completing the last recurrent activity opens
  it, so `div.page-encounter.prenatal` failing to appear IS the defect.

Related: [[e2e-local-run-procedure]], [[e2e-ci-partition-trap]], [[pre-push-code-review-gate]]
