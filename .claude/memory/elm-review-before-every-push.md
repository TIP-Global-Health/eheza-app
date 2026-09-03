---
name: elm-review-before-every-push
description: Run elm-review on the COMMITTED state via a local clone before every push of Elm changes — I have turned CI red three times by skipping it
metadata: 
  node_type: memory
  type: feedback
  originSessionId: c7019686-e437-4d8b-9f2d-a2a4c507d45d
  modified: 2026-08-30T00:00:00.000Z
---

⛔ **Run `elm-review` before every push that touches Elm.** Not "when I remember", not "on the
branch I think is risky". I skipped it three times in one session (2026-07-29/30) and turned CI
red twice — both times the user had to tell me (*"CI fails"*).

**Why:** the failures are always the same shape and always invisible to `elm make` and `elm-test`,
which both pass happily: **imports left behind when their only user was deleted.** Refactoring
that removes a call site is exactly the work I do most, so this is the most likely way I break CI.
Real instances: `ageInMonths` after moving an age check to Utils (#2005); `FloatInputConstraints`
+ `Page(..)`/`UserPage(..)` after a field and a test were rewritten (#2020);
`birthWeightOutsideConstraints` + `RangedMeasurement(..)` after an inline list was replaced (#2021).

**How to apply — the procedure that works:**

1. Commit locally. **Do not push yet.**
2. Clone the local repo at that branch, which reads the committed state:
   ```bash
   D=<scratchpad>/rev && rm -rf $D
   git clone -q --branch <branch> file:///var/www/html/ihangane $D
   cd $D/client && ln -s /var/www/html/ihangane/client/node_modules node_modules
   npx gulp version >/dev/null 2>&1          # else elm-review dies on Version.elm
   npx elm-review --compiler "$(which elm)"
   ```
3. Push only on `I found no errors!`.

The clone is needed because a direct local run dies on the gitignored duplicate `module Config`
(`Config.Deploy.elm`), and the `file://` form is needed for `--branch` to work. The `gulp version`
step matters: a stale `src/generated/Version.elm` makes elm-review abort with a parse error before
it reviews anything, which reads like a pass if the tail is not checked.

⚠ It must run on **the commit being pushed** — reviewing an earlier commit and then pushing more
on top is how #2015 went red. See [[verify-by-running-not-reasoning]],
[[pr-first-review-workflow]], [[elm-fulltest-needs-elm-make-main]].

⚠ **A stale `client/elm-stuff` makes elm-review report a PHANTOM unused import** (2026-08-30, B-212
worktree). It flagged `NoUnused.Variables: Imported module 'Backend.Entities' is not used` in
`Pages/Prenatal/ProgressReport/View.elm` — a file that uses `PrenatalEncounterId` at :144, on a
commit CI had already passed. The same worktree had reported *"I found no errors!"* on the same
content minutes earlier; an `elm-test` run in between is the suspect. **Discriminate before
believing it:** check out develop's OWN copy of the file into the same worktree (it still errored)
and run the identical develop client in another worktree (clean) — then
`rm -rf client/elm-stuff client/review/elm-stuff` and re-run, which cleared it. So a finding that
contradicts a green CI run is a cache artifact until a cleared cache reproduces it. The rule above
is unchanged: still run it, still on the committed state.

