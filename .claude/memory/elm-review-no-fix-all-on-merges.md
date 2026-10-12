---
name: elm-review-no-fix-all-on-merges
description: After merging develop into a long-stale branch, do NOT run `elm-review --fix-all` — it deletes wildcard imports the compiler actually needs (false NoUnused.Variables positives that survive cache clearing). Resolve elm-review issues file-by-file from compile errors, not en masse.
metadata:
  node_type: memory
  type: feedback
  originSessionId: 92b8c1e4-ab05-4a84-850f-6fbb1235d5b9
---

After merging `develop` into a long-stale branch, **do not run `npx elm-review --fix-all`**. Resolve issues file-by-file based on what `elm make` and individual `elm-review` reports flag, not via blanket auto-fix.

For a merge: get `elm make src/elm/Main.elm --output=/tmp/elm_out.js` clean using only manual edits (resolve conflicts, restore lost imports, fix renamed exports). Push and let CI report `lint_elm_review` issues. Address them as a follow-up commit if needed, narrowing one file at a time.

**Re-run elm-review on the *final* state before pushing.** elm-review is slow, so it is tempting to run it once at a "baseline" mid-task. Don't trust that — if you make further Elm changes after that run, the baseline is stale, and CI (which lints the final state) will catch what you missed. After the last Elm edit, re-run elm-review. Cost of skipping this: PR #1744's `lint_elm_review` failed on a `Test.elm` that was expanded *after* the local baseline run.

If you do run `elm-review` (without `--fix-all`) for inspection, clear cache first and pre-stage the two locally-generated false positives:

```bash
cd client
rm -rf elm-stuff
mv src/elm/Config.Deploy.elm /tmp/Config.Deploy.elm.bak 2>/dev/null
mv src/generated /tmp/generated.bak 2>/dev/null
npx elm-review                              # report only — no --fix-all
mv /tmp/Config.Deploy.elm.bak src/elm/Config.Deploy.elm 2>/dev/null
mv /tmp/generated.bak src/generated 2>/dev/null
```

**Why:** Tried `--fix-all` (with cache cleared) on two consecutive develop-merges (PR #1411 and PR #1653). On #1411 it applied 38 safe fixes after cache clear and worked. On #1653 it deleted ~10 wildcard imports of `Backend.Entities` and `Backend.Measurement.Model` even with cache cleared — those imports *were* used (provided types like `HealthCenterId`, `NurseId`, `PersonId`, `ColorAlertIndication`, `ReasonOtherReasons`, etc., transitively referenced in record fields and case patterns). Cost: hours of whack-a-mole `import` restoration, restored as wildcards which then violate `NoUnused.Variables` again. The "clear cache and re-run" rule from the previous lesson is **insufficient** — it works for branches where wildcard imports cover few names, fails for branches where they cover many. The deciding factor isn't cache freshness; it's whether `NoUnused.Variables`' analysis can see all the transitive uses at all (it sometimes can't, regardless of cache state).

**How to apply:**
1. For merge commits: focus on conflict resolution and `elm make` cleanup. Do not run `--fix-all`.
2. If `lint_elm_review` fails on CI after the merge push, fix in a separate commit. Approach each flagged file individually:
   - For `NoUnused.Variables: Imported module X is not used` on `Backend.Entities` / `Backend.Measurement.Model` / other modules that genuinely provide types used in records or constructors — verify the claim by removing the import and running `elm make`. If the compiler is happy, elm-review was right; if it complains, the wildcard import IS needed and you should add the file to `NoUnused.Variables.rule`'s ignore list in `review/src/ReviewConfig.elm` rather than narrowing.
   - For `NoUnused.Variables: Imported type X is not used` on a specific named import (e.g. `UserPage`, `Maybe`, `ReasonForNotConsenting`) — that's a real finding, drop the unused name.
   - For `NoUnused.Exports` / `NoMissingTypeAnnotation` / `NoPrematureLetComputation` — usually safe individual fixes.
3. **Never use `--fix-all` to clean up after a merge.** It bundles too many decisions and the false positives bury you.

**Related repo facts (verify before relying on them):**
- `review/src/ReviewConfig.elm` exempts these modules from `NoImportingEverything` (so `exposing (..)` is *intentional* for them): `Backend.Entities`, `Backend.Measurement.Model`, `Html`, `Html.Attributes`, `Html.Events`, `Svg`, `Svg.Attributes`, `SyncManager.Model`. But these are **not** in the `NoUnused.Variables.rule` ignore list — so wildcard imports of them are still flagged as "unused" when the rule's analysis misses transitive references.
- `NoUnused.Variables.rule` ignore list includes `Backend/Measurement/Model.elm`, `Backend/NutritionEncounter/Utils.elm`, `App/Model.elm`, `Pages/AcuteIllness/Activity/Utils.elm`. Add files here when wildcard imports are genuinely needed.
- `NoUnused.Exports.rule` ignore list includes `LocalConfig.Example.elm`, `Config.Deploy.elm`, `App/Model.elm`.
- `NoMissingSubscriptionsCall.rule` flags any file calling a `Module.update` without also calling that module's `subscriptions` — including unit tests. `update`-level test files must be added to its `ignoreErrorsForFiles` list in `review/src/ReviewConfig.elm` (e.g. `src/elm/SyncManager/Test.elm`, added on PR #1744).
