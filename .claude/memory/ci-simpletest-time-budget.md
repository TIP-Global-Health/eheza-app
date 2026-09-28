---
name: ci-simpletest-time-budget
description: E-Heza CI job durations + the 25-min budget; why adding a SimpleTest class is expensive and E2E is the real long pole
metadata: 
  node_type: memory
  type: reference
  originSessionId: 90b8cb68-cb52-4552-a1b0-2c0ef85b1ec0
---

**Goal (user, 2026-07-08):** keep CI run time under **25 minutes**.

**Per-job durations** (CircleCI `build_time_millis`, measured on clean PR #1909, 2026-07-08; representative):

| Job | Duration | Notes |
|---|---|---|
| e2e_playwright_2 | **30.0 min** | single longest job = overall CI critical path |
| e2e_reporting | 27.3 min | over 25 |
| e2e_playwright_1 | 24.8 min | at the line |
| test_simpletest_linux | 22.9 min | ~2 min headroom under 25; 4th longest, NOT critical path |
| test_elm | 1.3 min | |
| lint_elm_review | 1.2 min | |
| lint_elm | 0.7 min | |
| lint_phpcs | 0.6 min | |
| lint_shellcheck | 0.2 min | |
| test_zscore | 0.1 min | |

Structure: lints run first (~1 min), then simpletest + e2e run in parallel → **overall wall-clock ≈ 30 min, set by e2e_playwright_2**, not simpletest. So a CI-wide 25-min goal is currently blown by the **E2E trio**, not simpletest — fix by sharding e2e specs across more parallel jobs / splitting e2e_playwright_2. The 25-min concern the user raises about simpletest is a per-job target (keep it off the critical path).

**Why adding a SimpleTest is costly:** `HedleyWebTestBase::setUp` (server/hedley/HedleyWebTestBase.inc) does a fresh install + always-merges admin_menu/restful/restful_token_auth/hedley_device/hedley_restful + a full `features_revert()` — and DrupalWebTestCase reinstalls **per test METHOD** (so both a new CLASS and a new METHOD pay a full install). 7 test classes today. `ddev simpletest` runs `run-tests.sh --concurrency 4`; at concurrency 4, 7 classes = 2 waves (4+3), 8 = 2 waves (4+4), so an 8th *lightweight* class MIGHT fill an idle slot (~0 wall-clock) or land on the critical path (~+2-3 min) — measure, don't assume.

**Rule of thumb (learned removing the B-044 test, [[improvement-backlog]] B-044):** for a simple invariant/guard, prefer out-of-band verification (SQL demo on real tables in a rolled-back txn + a standalone guard-logic php check) over a committed integration test. If a committed regression test IS warranted, FOLD it as a new scenario into an existing class's existing METHOD (the B-032 pattern — no new install) rather than adding a class/method. The 7 existing classes: HedleyStatsCalculation, HedleyReportsDataEndpoint, HedleyActivityZscoreUpdate, HedleyRestfulBulkPhotosTest, HedleyUserAccessControlTests, HedleyStockManagementEndpoints, HedleyAdminIntegrityTests.

CircleCI durations without a token: job number from the status URL → `curl -s https://circleci.com/api/v1.1/project/github/TIP-Global-Health/eheza-app/<job>` → `.build_time_millis`.
