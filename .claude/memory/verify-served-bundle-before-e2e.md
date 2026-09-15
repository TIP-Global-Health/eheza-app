---
name: verify-served-bundle-before-e2e
description: "An e2e run proves nothing until the served Main.js is confirmed to hold the change — gulp serves a stale bundle after a branch checkout while reporting port 3000 up"
metadata:
  type: feedback
---

⛔ **Before believing any local e2e run, read the changed function out of `localhost:3000/Main.js` and confirm it is the build you think it is.**

**Why:** after checking a branch out in the main tree, `ddev gulp` answers on port 3000 within
~10 s **with the previous branch's bundle**. On 2026-09-08 the first read on the B-288 branch still
showed the pre-fix `medicateForDiabetes` body; `touch`ing the changed `.elm` file triggered the
watcher and the fixed body appeared ~20 s later. A run started in that window silently exercises the
wrong code — and for a discrimination run it produces the *expected* answer for the wrong reason.

**How to apply:** `curl -s localhost:3000/Main.js`, find the compiled top-level name
(`$author$project$Pages$NCD$Utils$medicateForDiabetes`) and read its body. This works because the
client ships **dev-mode** Elm — `--optimize` is blocked by `Debug.todo` in `Utils/AllDict.elm`
([[client-cannot-use-elm-optimize]]) — so every top-level name survives into the bundle. It is the
only signal for a logic-only change that adds no new strings. Poll until the body changes, and
`touch` the source file if the watcher has not noticed.

Related: [[e2e-local-run-procedure]], [[verify-by-running-not-reasoning]], [[local-verification-vs-ci]]
