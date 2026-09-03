---
name: elm-fulltest-needs-elm-make-main
description: "elm-test only compiles test-reachable modules — run `elm make src/elm/Main.elm` for a true full type-check after any app-wide Elm change"
metadata: 
  node_type: memory
  type: project
  originSessionId: 1233dca7-45c1-4582-8ca9-f04705bea163
---

In eheza-app's Elm client, **`elm-test` is NOT a full type-check.** It only compiles the modules reachable from the test files (`src/elm/**/Test.elm`) and their imports. Modules not in that dependency graph (e.g. `Pages.PinCode.View`) are never compiled by elm-test, so a type error or undefined variable in them passes elm-test green.

**After any app-wide change — especially changing a shared function's signature (e.g. threading a new parameter through call sites) — run a full app compile, not just elm-test:**

```
cd client && elm make src/elm/Main.elm --output=/dev/null
```

Success prints `Success! Compiled 548 modules` (≈548 as of 2026-06). This compiles the real entry point and catches everything elm-test misses.

Concrete miss (R2-4, PR #1832): threading a `Time.Zone` param through `fromLocalDateTime` call sites left `PinCode/View`'s top-level `viewLoggedInContent` referencing an out-of-scope `zone`; `elm-test` stayed green (2725 passed) because `PinCode.View` isn't test-reachable. Only `elm make Main.elm` flagged it.

Tooling notes: use project-local `client/node_modules/.bin/elm-test` (0.19.1-revision6) — the host-global elm-test is too new (wants elm-explorations/test 2.x). See [[code-review-improvement-ledger]] for the elm-review `Config.Deploy.elm` workaround.
