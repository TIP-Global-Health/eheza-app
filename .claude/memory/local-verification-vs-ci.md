---
name: local-verification-vs-ci
description: User is fine with relying on CI for the full test suites; local verification should be the compile plus whatever discriminates the specific change
metadata:
  type: feedback
---

Relying on CI for the full suites is fine (user, 2026-08-19, asked after PR #2111 how I verify client changes). Do not burn local time re-running `elm-test` / e2e / simpletest just to repeat what CI runs on the PR.

**Why:** CI already runs `test_elm` (2978 tests), e2e and simpletest on every PR branch. Local repetition of those adds latency without adding information; what CI *cannot* supply is the check that discriminates the specific change.

**How to apply:** locally run (a) `elm make src/elm/Main.elm` — the real type-check, since [[elm-fulltest-needs-elm-make-main]] `elm-test` alone only compiles test-reachable modules; (b) `elm-format --validate` and `elm-review` (see [[elm-review-before-every-push]]); (c) the check that actually discriminates — byte-`diff` of moved bodies for a pure relocation, a fail-first discrimination test for logic ([[verify-by-running-not-reasoning]]). When a tool reports pre-existing noise, get a baseline by stashing the edit and re-running in the same worktree rather than assuming. Then push and let CI do the rest.
