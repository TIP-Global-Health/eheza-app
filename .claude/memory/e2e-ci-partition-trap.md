---
name: e2e-ci-partition-trap
description: "A new Playwright test can run in NO CI job — e2e_playwright_1 excludes titles matching its --grep-invert list, and the other jobs filter by file path"
metadata:
  node_type: memory
  type: feedback
---

A green e2e run proves nothing about a new test until you check the test is inside the CI
partition. `.circleci/config.yml` splits the suite three ways:

- `e2e_playwright_1`: `playwright test prenatal tuberculosis`, then everything else under
  `--grep-invert "Well Child|NCD|HIV|Tuberculosis|Child Scoreboard|Prenatal|Family Nutrition|
  Group Nutrition|Group Education|Stock Management|Admin Reports|Bulk Photo Fetch"`
- `e2e_playwright_2`: a list of **file-path** tokens (`well-child ncd hiv child-scoreboard …`)
- `e2e_reporting`: `reporting`

**Why:** on B-323 (PR #2210) I added a lab-tech case titled "…a positive partner **HIV**
result…" to `lab-tech-encounter.spec.ts`. The title matched job 1's `--grep-invert` "HIV", and the
file matches no token in job 2's list — so it ran in zero jobs while all three e2e checks went
green. Caught only by listing the CI commands myself, after reporting the PR as covered.

**How to apply:**
- After adding or renaming an e2e test, run each of the three CI commands with `--list` and grep
  for the new test. A title containing any `--grep-invert` word puts it outside job 1.
- ⚠ `playwright test <token>` matches the token against the **ABSOLUTE** path. A worktree named
  `B-323-partner-hiv-labtech-note` contains "hiv", so `playwright test hiv --list` matches every
  spec in the repo and the measurement lies. Measure from a neutrally-named directory —
  symlink `node_modules`, `playwright.config.ts`, `package.json` and `e2e` into the scratchpad.
  Sanity-check with a token that occurs only in the worktree name.
- Prefer extending a test that already runs over adding a new one: both e2e jobs sit at 30–32
  minutes, so a second full encounter is expensive. See [[ci-simpletest-time-budget]].
- The standing fix for this class is backlog item **G-04** (E2E spec-partition guard).

Related: [[e2e-local-run-procedure]], [[verify-by-running-not-reasoning]]
