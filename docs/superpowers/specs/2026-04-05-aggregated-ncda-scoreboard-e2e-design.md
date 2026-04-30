# Aggregated NCDA Scoreboard E2E Test Design

## Overview

Add end-to-end tests for the Aggregated NCDA Scoreboard report — the third section of admin reports at `/admin/reports/aggregated-ncda`. Tests verify the full pipeline: NCDA data collection in the PWA, sync, backend aggregation (Advanced Queue + batch scripts), and scoreboard rendering in the server-side Elm app.

Tests are added as new phases in the existing `reporting.spec.ts`, leveraging encounters already created by that test.

## Architecture

### Data Pipeline Under Test

```
Client NCDA form (4 encounter types)
  -> Sync to Drupal
  -> hook_node_insert triggers AQ task
  -> AQ worker: hedley_ncda_calculate_aggregated_data_for_person()
  -> JSON stored in person.field_ncda_data
  -> generate-data-for-all.php (batch, --exclude_set for delta)
  -> recalculate-large-datasets.php (pre-computes district Report Data nodes)
  -> Server Elm app reads JSON via Drupal flags, renders scoreboard
```

### NCDA Content Types Tested

All 4 NCDA bundles that contribute to aggregated data:

- `child_scoreboard_ncda` — via CSChild (CHW, Akanduga village)
- `nutrition_ncda` — via NutrChild (Nurse, Nyange HC)
- `well_child_ncda` — via NutrChild Well Child encounter (Nurse)
- `group_ncda` — via FBF Group Child (Nurse, Nyange HC)

### Geographic Hierarchy

Nyange Health Center persons are in:
- Province: Amajyaruguru
- District: Gakenke
- Sector: Coko
- Cell: Mbirima (CHW village: Akanduga)

Scoreboard URLs:
- District: `/admin/reports/aggregated-ncda/Amajyaruguru/Gakenke`
- Village: `/admin/reports/aggregated-ncda/Amajyaruguru/Gakenke/Coko/Mbirima/Akanduga`

## Test Phases

### Phase 0 (Modified) — Baseline Setup

After existing `recalculateLargeDatasets()`, add:

```
ensureNCDAFeatureEnabled();       // drush vset hedley_admin_feature_ncda_enabled 1
generateNCDAPersonData(false);    // generate-data-for-all.php (all persons)
ncdaRecalculateLargeDatasets();   // hedley_ncda recalculate-large-datasets.php + cc all
```

### Phase 0 — Baseline Capture (New Sub-step)

Navigate to Drupal admin scoreboard. Record baseline values:

1. **District level** (Gakenke): read Demographics pane (Children Under 2 for current month).
2. **Village level** (Akanduga): read all 8 panes baseline. Verify structural correctness:
   - All 8 data panes visible
   - Entity name displays correctly
   - 12 month columns present
   - Top bar (year selector, values/percentages toggle) renders

### Phase 1 Encounters — NCDA Answer Modifications

No new encounters are added. The existing encounters are modified to produce verifiable NCDA data.

#### CSChild (Child Scoreboard NCDA) — Changes from current

MUAC: 14.0 -> **12.0** (moderate range, triggers TreatedForAcuteMalnutrition question).

Pane 4 Targeted Interventions — all changed from No to Yes/No mix:
- ChildReceivesFBF -> **Yes**, ChildTakingFBF -> **Yes** (pane4.row1)
- TreatedForAcuteMalnutrition -> **Yes** (pane4.row2, now visible due to MUAC change)
- ChildWithDisability -> **Yes**, ReceivingSupport -> **Yes** (pane4.row4)
- BeneficiaryCashTransfer -> **No** (pane4.row5, deliberately No)
- ConditionalFoodItems -> **No** (pane4.row6, deliberately No)
- ChildGotDiarrhea -> **Yes** (NCDA sign, but NOT pane4.row3 — that needs ORS/Zinc)

Deliberate No answers for negative-path verification:
- ECD (pane2.row5) -> **No**
- MealsAtRecommendedTimes (pane3.row4) -> **No**
- BeneficiaryCashTransfer/ReceivingCashTransfer (pane4.row5) -> **No**
- ConditionalFoodItems (pane4.row6) -> **No**
- InsecticideTreatedBednets (pane5.row4) -> **No**
- HasKitchenGarden (pane5.row5) -> **No**

#### Group NCDA (FBF Group Child) — Changes from current

Currently answers everything "No". Change to answer Yes for the rows CSChild answers No, creating a complementary pattern:

- ECD -> **Yes** (pane2.row5)
- MealsAtRecommendedTimes -> **Yes** (pane3.row4)
- BeneficiaryCashTransfer -> **Yes**, ReceivingCashTransfer -> **Yes** (pane4.row5)
- ConditionalFoodItems -> **Yes** (pane4.row6)
- InsecticideTreatedBednets -> **Yes** (pane5.row4)
- HasKitchenGarden -> **Yes** (pane5.row5)
- All other questions remain No

#### NutrChild (Nutrition NCDA + Well Child NCDA)

No changes. Current answers (mostly Yes) are retained.

### Phase 2 (Modified) — Post-Encounter Aggregation

After existing `processAdvancedQueue()` and `recalculateLargeDatasets()`, add:

```
ncdaRecalculateLargeDatasets();   // Re-compute district-level NCDA Report Data
```

AQ processing already handles per-person `field_ncda_data` computation via `hook_node_insert` triggers. No `generateNCDAPersonData()` call needed here.

### Phase 16 (New) — NCDA Scoreboard Verification

#### Step 1: Village-Level Verification (Akanduga)

Navigate to village scoreboard. Read all panes. Assert deltas against baseline:

**Demographics (Pane — cyan):**
- Children Under 2: +1

**Acute Malnutrition (Pane — orange):**
- Moderate Acute Malnutrition: +1 (MUAC 12.0)

**Stunting (Pane — velvet):**
- No Stunting: +1 (green)

**ANC & Newborn (Pane — cyan):**
- Iron During Pregnancy (row2): +1

**Universal Interventions (Pane — orange):**
- Vitamin A (row2): +1
- Dewormer (row3): +1
- Ongera MNP (row4): +1
- ECD (row5): **+0** (answered No)

**Nutrition Behavior (Pane — velvet):**
- Breastfed 6 months (row1): +1
- Complementary feeding (row2): +1
- Five food groups (row3): +1
- Meals at recommended times (row4): **+0** (answered No)

**Targeted Interventions (Pane — cyan):**
- FBF (row1): +1
- Malnutrition treatment (row2): +1
- Support (row4): +1
- Cash transfer (row5): **+0** (answered No)
- Conditional food items (row6): **+0** (answered No)

**Infrastructure/WASH (Pane — orange):**
- Clean water (row1): +1
- Toilets (row2): +1
- Handwashing (row3): +1
- Bednets (row4): **+0** (answered No)
- Kitchen garden (row5): **+0** (answered No)

#### Step 2: District-Level Verification (Gakenke)

Navigate to district scoreboard. Assert:

**Structural:** All 8 panes render with valid data.

**Demographics:** Children Under 2 increased. The exact delta depends on how many nurse-created children land in Gakenke district (nurse registration uses `selectByLabel('Province:', 1)` which picks the first dropdown option — may not be Amajyaruguru). At minimum, CSChild (+1) and HVChild (+1) are in Gakenke (CHW at Akanduga). Assert delta >= 2.

**Complementary coverage:** Rows that CSChild answered No to (ECD, meals, cash transfer, conditional food items, bednets, kitchen garden) should show increases at district level from the Group Child's Yes answers — proving `group_ncda` data flows through the pipeline. This assertion only applies if the Group Child is in Gakenke district (depends on nurse registration address). If not verifiable, skip these district-level complementary assertions and rely on village-level verification for positive/negative path coverage.

### Rows Excluded from Delta Assertions

| Row | Reason |
|-----|--------|
| Pane 1 Row 1 (ANC visits >= 4) | Requires 4+ prenatal encounters linked to child |
| Pane 2 Row 1 (Immunization on-track) | Complex computed logic with vaccine schedules |
| Pane 4 Row 3 (Diarrhea treatment) | Requires ORS/Zinc medication distribution from acute illness |

## Helper Functions

New functions in `client/e2e/helpers/reports.ts`:

### ensureNCDAFeatureEnabled()

Runs `drush vset hedley_admin_feature_ncda_enabled 1` to ensure the NCDA feature flag is active. Uses `execSync` with the same `drushEnv()` pattern as existing helpers. No user input involved.

### generateNCDAPersonData(excludeSet: boolean)

Runs `generate-data-for-all.php` via drush scripting. When `excludeSet` is true, passes `--exclude_set=1` to skip persons that already have `field_ncda_data`. Timeout: 300s.

### ncdaRecalculateLargeDatasets()

Runs `hedley_ncda/scripts/recalculate-large-datasets.php` via drush scripting, then `drush cc all` to clear caches. This pre-computes district-level Report Data nodes. Timeout: 300s.

### navigateToNCDAScoreboard(page, geoPath: string)

Navigates to `{ddevUrl}/admin/reports/aggregated-ncda/{geoPath}`. Waits for `.page-content` with `.pane` elements to render. Retries with `page.reload()` if "Not Found" appears (lazy data fetch pattern).

### readScoreboardPane(page, paneIndex: number)

```typescript
interface ScoreboardPaneData {
  heading: string;
  rows: { label: string; values: string[] }[];
}
```

Locates `.pane` elements by index (0=entity info, 1=demographics, ..., 8=infrastructure). Reads `.pane-heading` text, iterates `.table-row` children reading `.cell.activity` (label) and `.cell.value` (values).

### findScoreboardValue(pane, rowLabel, monthIndex)

Finds a specific cell value by row label substring and 0-based month index. Returns the string value or empty string if not found.

## File Changes Summary

| File | Change |
|------|--------|
| `client/e2e/helpers/reports.ts` | Add 5 new helper functions |
| `client/e2e/helpers/child-scoreboard.ts` | Modify `completeNCDA()`: MUAC 12.0, Pane 4 Yes answers, deliberate No answers |
| `client/e2e/helpers/group-session.ts` | Modify `completeNCDA()`: targeted Yes answers instead of all-No |
| `client/e2e/reporting.spec.ts` | Modify Phase 0 (NCDA baseline), Phase 2 (NCDA recalc), add Phase 16 |
| `.circleci/config.yml` | No changes needed — reporting tests already run in dedicated `e2e_reporting` job |

## Scoreboard HTML Structure (for selectors)

```html
<div class="page-content">
  <div class="top-bar">
    <div class="new-selection">...</div>
    <div class="year-selector">...</div>
    <div class="values-percents">...</div>
  </div>
  <div class="pane">                    <!-- index 0: entity info -->
    <div class="pane-heading">Aggregated Child Scoreboard</div>
    <div class="pane-content">
      <span class="selected-entity">Village:</span>
      <span>Akanduga</span>
    </div>
  </div>
  <div class="pane cyan">              <!-- index 1: demographics -->
    <div class="pane-heading">Demographics</div>
    <div class="pane-content">
      <div class="table-header">
        <div class="cell activity">Status</div>
        <div class="cell">Jan</div>...<div class="cell">Dec</div>
      </div>
      <div class="table-row">
        <div class="cell activity">Children under 2</div>
        <div class="cell value">5</div>...
      </div>
      ...more rows
    </div>
  </div>
  <!-- 7 more panes with same structure -->
</div>
```

Pane order (0-indexed): entity info (0), demographics (1), acute malnutrition (2), stunting (3), ANC/newborn (4), universal interventions (5), nutrition behavior (6), targeted interventions (7), infrastructure/WASH (8).
