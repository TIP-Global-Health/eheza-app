---
name: local-verification-vs-ci
description: User is fine with relying on CI for the full test suites; local verification should be the compile plus whatever discriminates the specific change
metadata:
  type: feedback
---

Relying on CI for the full suites is fine (user, 2026-08-19, asked after PR #2111 how I verify client changes). Do not burn local time re-running `elm-test` / e2e / simpletest just to repeat what CI runs on the PR.

**Why:** CI already runs `test_elm` (2978 tests), e2e and simpletest on every PR branch. Local repetition of those adds latency without adding information; what CI *cannot* supply is the check that discriminates the specific change.

**How to apply:** locally run (a) `elm make src/elm/Main.elm` — the real type-check, since [[elm-fulltest-needs-elm-make-main]] `elm-test` alone only compiles test-reachable modules; (b) `elm-format --validate` and `elm-review` (see [[elm-review-before-every-push]]); (c) the check that actually discriminates — byte-`diff` of moved bodies for a pure relocation, a fail-first discrimination test for logic ([[verify-by-running-not-reasoning]]). When a tool reports pre-existing noise, get a baseline by stashing the edit and re-running in the same worktree rather than assuming. Then push and let CI do the rest.

## ⛔ A passing CI job is not evidence a scenario ran (R25, 2026-08-25)

`HedleyStatsCalculation.test` has three scenarios — including the regression tests for B-032 and
B-033 — that exit early and never execute. The method builds a request without the `statistics`
key, so the stats payload is never produced, `end($results['batch'])` is not an array, and a bare
`return` skips everything after it. It has been silently green since `36d7354ae` (2020), a commit
titled *"Make Travis pass"* which deleted the `assertTrue(is_array($stats))` that would have failed
loudly — leaving behind a guard whose comment still says "the above test will fail".

Two backlog entries (B-032, B-033) recorded "CI green incl. test_simpletest_linux (new scenario ran
end-to-end)" on the strength of that green. **The job passing proved only that nothing threw.**

The check that discriminates: assert on a value the code under test actually produces. The dead
assertion here was `assertFalse(empty($stats['good_nutrition']))`, and `good_nutrition` appears
nowhere else in the repository — a one-line grep would have exposed it at any point in six years.
When adding a scenario to an existing test method, confirm it runs by making it **fail first**.
