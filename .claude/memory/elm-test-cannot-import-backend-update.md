---
name: elm-test-cannot-import-backend-update
description: ⛔ a Test.elm importing Backend.Update OOM-kills CI's test_elm ("elm make failed with exit code null") — test via a seam in a Pages/*/Utils module instead
metadata:
  type: project
---

A test module that imports `Backend.Update` pulls essentially the whole 548-module
application graph into the **elm-test** compile, and CI's `test_elm` container kills the
compiler: the job log ends with `` `elm make` failed with exit code null `` — a killed
process, no compile errors. It compiles fine locally (more memory), so local green and a
fresh-clone elm-review pass prove nothing about this failure. Two identical deaths on
PR #2185 (2026-09-01) before the cause was found; an empty-commit retrigger was wasted on
the "flake" theory first.

**Why:** exit code null in node = process killed (OOM), and `Backend.Update` transitively
imports nearly every module including all Pages views. Other CI jobs compile `Main.elm`
fine; elm-test's generated project on top of that graph is what crosses the limit.

**How to apply:** to unit-test logic that lives in `Backend/Update.elm`, move the decision
into a pure function in the owning `Pages/<program>/…/Utils.elm` (the seam), have
`Backend.Update` map its result to messages, and test the seam from that program's existing
`Test.elm` — those graphs CI already compiles. B-280 is the worked example:
`subsequentEncounterDiagnosisUpdate` in `Pages/AcuteIllness/Activity/Utils.elm`, tested in
`Pages/AcuteIllness/Activity/Test.elm` via `testAssembled`. Related: [[verify-by-running-not-reasoning]],
[[elm-review-before-every-push]].
