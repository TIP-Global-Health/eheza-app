# Aggregated NCDA Scoreboard E2E Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add E2E tests verifying the full Aggregated NCDA Scoreboard pipeline — from NCDA data collection in the PWA through backend aggregation to scoreboard rendering in the Drupal admin Elm app.

**Architecture:** Extend the existing `reporting.spec.ts` with NCDA scoreboard baseline capture (Phase 0), post-encounter aggregation (Phase 2), and a new Phase 16 for scoreboard verification at village and district levels. Modify NCDA answers in child-scoreboard and group-session helpers to produce a mix of Yes/No values for both positive and negative path verification.

**Tech Stack:** Playwright, TypeScript, Drupal 7 drush scripts, Elm (server-side)

**Spec:** `docs/superpowers/specs/2026-04-05-aggregated-ncda-scoreboard-e2e-design.md`

---

### Task 1: Add NCDA backend helper functions to reports.ts

**Files:**
- Modify: `client/e2e/helpers/reports.ts`

- [ ] **Step 1: Add three new functions after `ensurePrenatalMedicationsVariable` (line ~124)**

Add `ensureNCDAFeatureEnabled()` — runs `drush vset hedley_admin_feature_ncda_enabled 1`. Follow the exact same pattern as `ensurePrenatalMedicationsVariable()` — use `execSync` with `drushEnv()`, hardcoded command (no user input), console.log before and after.

Add `generateNCDAPersonData(excludeSet = false)` — runs `drush scr profiles/hedley/modules/custom/hedley_ncda/scripts/generate-data-for-all.php` with optional `--exclude_set=1` flag. Follow the exact same pattern as `generateBaseReportsData()` — use `execSync` with 300s timeout.

Add `ncdaRecalculateLargeDatasets()` — runs `drush scr profiles/hedley/modules/custom/hedley_ncda/scripts/recalculate-large-datasets.php` then `drush cc all`. Follow the exact same pattern as `recalculateLargeDatasets()`.

All three functions use hardcoded commands with no user-provided data, matching the security pattern of existing helpers.

- [ ] **Step 2: Commit**

```bash
git add client/e2e/helpers/reports.ts
git commit -m "Add NCDA backend helper functions (feature flag, data generation, recalculation)"
```

---

### Task 2: Add NCDA scoreboard navigation and table reading helpers to reports.ts

**Files:**
- Modify: `client/e2e/helpers/reports.ts`

- [ ] **Step 1: Add types and navigation function**

Add after the Task 1 functions:

Export `ScoreboardRow` interface: `{ label: string; values: string[] }`.

Export `ScoreboardPaneData` interface: `{ heading: string; rows: ScoreboardRow[] }`.

Export `navigateToNCDAScoreboard(page: Page, geoPath: string)` — navigates to `{getDdevUrl()}/admin/reports/aggregated-ncda/{geoPath}`. Waits for `.pane` elements, retries up to 5 times with `page.reload()` if panes don't appear (handles lazy data fetch "Not Found" pattern). Verifies at least 2 panes visible before returning.

- [ ] **Step 2: Add table reading functions**

Export `readScoreboardPane(page: Page, paneIndex: number): Promise<ScoreboardPaneData>` — locates `.pane` by nth(paneIndex), reads `.pane-heading` text, iterates `.table-row` children reading `.cell.activity` for label and `.cell.value` for 12 monthly values. Returns structured data.

Pane indices: 0=entity info, 1=demographics, 2=acute malnutrition, 3=stunting, 4=ANC/newborn, 5=universal interventions, 6=nutrition behavior, 7=targeted interventions, 8=infrastructure/WASH.

Export `findScoreboardValue(pane: ScoreboardPaneData, rowLabel: string, monthIndex: number): number` — finds row by substring match on label, returns parsed int value at month index (0-based, Jan=0), or 0 if not found/empty.

- [ ] **Step 3: Commit**

```bash
git add client/e2e/helpers/reports.ts
git commit -m "Add NCDA scoreboard navigation and table reading helpers"
```

---

### Task 3: Modify child-scoreboard NCDA answers

**Files:**
- Modify: `client/e2e/helpers/child-scoreboard.ts`

All changes are in the `completeNCDA()` function.

- [ ] **Step 1: Step 4 (Nutrition Assessment) — change MUAC from 14.0 to 12.0**

Change `muacInput.fill('14.0')` to `muacInput.fill('12.0')` (line ~404). Update comment to note moderate range triggers TreatedForAcuteMalnutrition visibility.

- [ ] **Step 2: Step 2 (Universal Interventions) — change ECD to No**

Change `answerNCDAYesNo(page, 'sing lullabies', 'Yes')` to `'No'` (line ~356). Update comment to note deliberate No for negative-path verification.

- [ ] **Step 3: Step 3 (Nutrition Behavior) — change MealsAtRecommendedTimes to No**

Change `answerNCDAYesNo(page, 'eat at the recommended times', 'Yes')` to `'No'` (line ~383). Update comment.

- [ ] **Step 4: Step 5 (Targeted Interventions) — rewrite with Yes/No mix**

Replace the entire Step 5 block (lines ~414-438). New answers:

- `receive FBF` → **Yes** (was No)
- `FBF being consumed` → **Yes** (new conditional, shown when FBF=Yes)
- `beneficiary of cash transfer` → **No** (unchanged)
- `other support` (ConditionalFoodItems) → **No** (unchanged)
- `child being treated` (TreatedForAcuteMalnutrition) → **Yes** (new question, visible due to MUAC 12.0)
- `have disability` → **Yes** (was No)
- `receive support` (ReceivingSupport) → **Yes** (new conditional, shown when disability=Yes)
- `have diarrhea` → **Yes** (was No)

Question label reference (from `Translate.elm` `NCDASignQuestion`):
- ChildReceivesFBF: "Did the child receive FBF"
- ChildTakingFBF: "Is FBF being consumed"
- TreatedForAcuteMalnutrition: "Is the child being treated"
- ChildWithDisability: "Does the child have disability"
- ReceivingSupport: "Does the child receive support"
- ChildGotDiarrhea: "Does the child have diarrhea"

- [ ] **Step 5: Step 6 (Infrastructure) — change Bednets and KitchenGarden to No**

Change `answerNCDAYesNo(page, 'kitchen garden', 'Yes')` to `'No'` (line ~457).
Change `answerNCDAYesNo(page, 'insecticide-treated bednets', 'Yes')` to `'No'` (line ~461).
Update comments to note deliberate No.

- [ ] **Step 6: Commit**

```bash
git add client/e2e/helpers/child-scoreboard.ts
git commit -m "Modify child-scoreboard NCDA answers for scoreboard coverage (Yes/No mix)"
```

---

### Task 4: Modify group-session NCDA to answer targeted questions Yes

**Files:**
- Modify: `client/e2e/helpers/group-session.ts`

- [ ] **Step 1: Replace `completeNCDA()` with step-by-step implementation**

The current implementation (lines 691-808) uses a generic loop that clicks all "No" buttons. Replace with a step-by-step approach that:

1. For each NCDA step: first answers all visible questions "No" (using the same loop pattern), then overrides specific questions to "Yes".
2. Questions to override to Yes (complementary to CSChild's No answers):
   - Step 2 (Universal): ECD ("sing lullabies") → Yes
   - Step 3 (Nutrition Behavior): MealsAtRecommendedTimes ("eat at the recommended times") → Yes
   - Step 5 (Targeted): BeneficiaryCashTransfer ("beneficiary of cash transfer") → Yes, then ReceivingCashTransfer ("Are they receiving it") → Yes, ConditionalFoodItems ("other support") → Yes
   - Step 6 (Infrastructure): InsecticideTreatedBednets ("insecticide-treated bednets") → Yes, HasKitchenGarden ("kitchen garden") → Yes

3. Add private helper `answerAllNoAndFillNumbers(page)` — clicks all visible "No" labels and fills empty number inputs with 3000 (extracted from the old loop).

4. Add private helper `overrideToYes(page, questionSubstring)` — finds the question label, navigates to its sibling `.form-input`, clicks "Yes". Uses the same `evaluate` pattern as `answerNCDAYesNo` in child-scoreboard.ts.

5. Add private helper `clickGroupNCDASave(page)` — clicks Save and dismisses any post-save overlay/modal (extracted from the old loop's overlay handling).

6. Keep the initial overlay/modal dismissal logic at the top of the function unchanged.

- [ ] **Step 2: Commit**

```bash
git add client/e2e/helpers/group-session.ts
git commit -m "Modify group-session NCDA to answer targeted questions Yes for scoreboard coverage"
```

---

### Task 5: Modify reporting.spec.ts Phase 0 — NCDA baseline setup and capture

**Files:**
- Modify: `client/e2e/reporting.spec.ts`

- [ ] **Step 1: Add imports**

Add to the imports from `'./helpers/reports'` (lines 6-49):
`ensureNCDAFeatureEnabled`, `generateNCDAPersonData`, `ncdaRecalculateLargeDatasets`, `navigateToNCDAScoreboard`, `readScoreboardPane`, `findScoreboardValue`, `ScoreboardPaneData`.

- [ ] **Step 2: Add NCDA setup to Phase 0 data generation step**

Inside the existing Phase 0 step (line ~247-252), after `recalculateLargeDatasets();`, add:

```typescript
      ensureNCDAFeatureEnabled();
      generateNCDAPersonData(false);
      ncdaRecalculateLargeDatasets();
```

- [ ] **Step 3: Add baseline variables**

After existing baseline declarations (around line ~269), add:

```typescript
    let baselineVillageScoreboard: ScoreboardPaneData[];
    let baselineDistrictDemographics: ScoreboardPaneData;
```

- [ ] **Step 4: Add baseline capture step**

After the last completion report baseline capture step (around line ~448), add a new step `'Record NCDA Scoreboard baselines'` that:

1. Ensures Drupal admin login (check if current URL contains `/admin/`, login if not)
2. Navigates to village scoreboard at `Amajyaruguru/Gakenke/Coko/Mbirima/Akanduga`
3. Reads all 9 panes (indices 0-8) into `baselineVillageScoreboard` array
4. Navigates to district scoreboard at `Amajyaruguru/Gakenke`
5. Reads pane 1 (demographics) into `baselineDistrictDemographics`
6. Logs baseline pane counts for debugging

- [ ] **Step 5: Commit**

```bash
git add client/e2e/reporting.spec.ts
git commit -m "Add NCDA scoreboard baseline setup and capture to Phase 0"
```

---

### Task 6: Modify reporting.spec.ts Phase 2 — add NCDA recalculation

**Files:**
- Modify: `client/e2e/reporting.spec.ts`

- [ ] **Step 1: Add NCDA recalculation to Phase 2**

In Phase 2 (line ~1066-1084), after `completionRecalculateLargeDatasets();`, add:

```typescript
      ncdaRecalculateLargeDatasets();
```

- [ ] **Step 2: Commit**

```bash
git add client/e2e/reporting.spec.ts
git commit -m "Add NCDA recalculation to Phase 2"
```

---

### Task 7: Add Phase 16 — NCDA Scoreboard verification

**Files:**
- Modify: `client/e2e/reporting.spec.ts`

- [ ] **Step 1: Add Phase 16 before the closing `});` of the test (line ~2329)**

Phase 16 has 3 steps:

**Step: Navigate to village scoreboard** — login to Drupal admin if needed, navigate to `Amajyaruguru/Gakenke/Coko/Mbirima/Akanduga`.

**Step: Read and verify village scoreboard** — read all 8 data panes. Create `assertDelta` helper (same pattern as completion report phases) that compares current value against baseline at `currentMonth` index (0-based, `new Date().getMonth()`).

Assert these deltas (using scoreboard row labels from `server/elm/src/Translate.elm`):

| Pane | Row label substring | Expected delta |
|------|---------------------|----------------|
| Demographics (1) | `Children under 2` | +1 |
| Acute Malnutrition (2) | `Moderate Acute Malnutrition` | +1 |
| Stunting (3) | `No Stunting` | +1 |
| ANC/Newborn (4) | `Iron during pregnancy` | +1 |
| Universal (5) | `Vitamin A` | +1 |
| Universal (5) | `Deworming` | +1 |
| Universal (5) | `Ongera` | +1 |
| Universal (5) | `ECD` | 0 |
| Nutrition Behavior (6) | `Breastfed` | +1 |
| Nutrition Behavior (6) | `complementary feeding` | +1 |
| Nutrition Behavior (6) | `Diverse diet` | +1 |
| Nutrition Behavior (6) | `frequency of meals` | 0 |
| Targeted (7) | `FBF` | +1 |
| Targeted (7) | `Treatment for acute malnutrition` | +1 |
| Targeted (7) | `support to a child` | +1 |
| Targeted (7) | `conditional cash transfer` | 0 |
| Targeted (7) | `conditional food items` | 0 |
| Infrastructure (8) | `clean water` | +1 |
| Infrastructure (8) | `toilets` | +1 |
| Infrastructure (8) | `handwashing` | +1 |
| Infrastructure (8) | `bed nets` | 0 |
| Infrastructure (8) | `kitchen garden` | 0 |

**Step: Verify district scoreboard** — navigate to `Amajyaruguru/Gakenke`. Assert 9 panes rendered, `Children under 2` delta >= 2, entity info heading contains "Aggregated Child Scoreboard".

- [ ] **Step 2: Commit**

```bash
git add client/e2e/reporting.spec.ts
git commit -m "Add Phase 16: NCDA Scoreboard verification at village and district levels"
```

---

### Task 8: Run the test and verify

**Files:**
- None (test execution only)

- [ ] **Step 1: Run the reporting spec in background**

```bash
cd client && ./node_modules/.bin/playwright test reporting
```

Show trace file path immediately after launching for `tail -f`.

- [ ] **Step 2: If test fails, debug without re-running**

For tests > 2 minutes, never re-run the full test to debug. Instead:
1. Check error screenshots/snapshots from `client/test-results/`
2. Read Elm source to verify CSS selectors and row labels
3. Use standalone drush commands to verify backend data exists
4. Only re-run when confident the fix is correct

- [ ] **Step 3: Fix issues and re-run if needed**

- [ ] **Step 4: Check if timeout needs increasing**

If the test exceeds 18 minutes, increase `test.describe.configure({ timeout: ... })` to 1200000 (20 minutes).

- [ ] **Step 5: Commit final state**

```bash
git add -A
git commit -m "Fix test issues from initial run"
```
