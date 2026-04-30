# Add MAM/SAM rows to Nutrition Statistical Queries

**Issue:** [TIP-Global-Health/eheza-app#1718](https://github.com/TIP-Global-Health/eheza-app/issues/1718)
**Date:** 2026-04-09

## Summary

Add two new rows — **MAM (Moderate Acute Malnutrition)** and **SAM (Severe Acute Malnutrition)** — to all 8 existing nutrition tables in the Statistical Queries report. Categorization is based on MUAC and edema, not z-scores.

- **MAM:** MUAC ≥ 115 mm and < 125 mm
- **SAM:** MUAC < 115 mm OR edema present
- **Normal:** MUAC ≥ 125 mm and no edema
- **Missing MUAC:** encounter does not enter the MAM/SAM denominator at all

The 8 tables remain unchanged in shape (period columns, prevalence/incidence variants); only two rows are appended to each.

## Background — current architecture

`hedley_reports` ships statistical queries via a hybrid pipeline:

- Per-person data is stored as JSON in `field_reports_data` on each `person` node and recalculated incrementally via Advanced Queue when relevant content changes (`hedley_reports_calculate_aggregated_data_for_person` in `server/hedley/modules/custom/hedley_reports/hedley_reports.module`).
- For **small scopes** (sector, cell, village) the admin Elm app at `server/elm/` loads raw per-person JSON and computes the 8 nutrition tables in the browser (`Pages/Reports/Utils.elm`).
- For **large scopes** (global, province, district, health center) PHP pre-computes the 8 tables via `hedley_reports_generate_nutrition_report_data` and stores them on a `report_data` node, refreshed by `recalculate-large-datasets.php`. The Elm app receives the pre-computed table strings under `additional.nutrition_report_data` and renders them directly.

This design must update **both** the PHP and Elm computation paths in lockstep, plus the wire format that connects them.

The current per-encounter wire format inside `field_reports_data`:

| Encounter type | Format | Source |
|---|---|---|
| Individual nutrition / Well-child / Group nutrition (5 group types) | `"YYYY-MM-DD s,u,w"` | `hedley_reports_nutrition_metrics_to_string` |
| Family nutrition | `"YYYY-MM-DD"` | n/a (date-only) |

`s,u,w` are stunting / underweight / wasting z-scores (the existing internal `'w'`/`'u'` PHP variable naming is misaligned with the wire-string position; this confusion is left untouched as out of scope).

Family nutrition encounters currently do **not** contribute to any of the 8 nutrition tables.

## Approach

Symmetrical extension. MAM and SAM become two more rows alongside stunting / wasting / underweight, populated by a new categorization helper. Every existing helper (`sumNutritionMetrics`, `generatePrevalence...`, `generateIncidence...`, `nutritionMetricsToTableData`, view rendering) grows two more parallel buckets. Family nutrition becomes a new data source contributing to MAM/SAM only.

This is the smallest change that satisfies the issue, keeps the PHP and Elm pipelines structurally identical, and avoids any new abstractions.

Two alternatives were rejected:

- **Refactor to a generic Indicator abstraction first** (parameterize categorize / sum / prevalence / incidence over an `Indicator` ADT). YAGNI — there is no second indicator on the horizon, and the refactor would balloon the diff in two languages with no scope-relevant benefit.
- **Backend-only computation, dropping the in-browser path for MAM/SAM.** Introduces inconsistency (some rows computed in browser, some pre-computed) and forces the heavy precompute job to run for sector/cell/village scopes too, defeating the original reason small scopes compute on the fly.

## Section 1 — Data capture & wire format

### Per-encounter aggregation (PHP)

`hedley_reports_calculate_aggregated_data_for_person` builds a new `$muac_edema_by_encounter` map keyed by encounter id, populated from three additional measurement sources:

- **`HEDLEY_ACTIVITY_MUAC_BUNDLES`** — covers `nutrition_muac` (→ `field_nutrition_encounter`), `well_child_muac` (→ `field_well_child_encounter`), and `muac` (→ `field_session`). Each contributes `field_muac` (cm float).
- **`HEDLEY_ACTIVITY_NUTRITION_BUNDLES`** — covers `nutrition` and `well_child_nutrition`. Each contributes the edema flag: `e = in_array('edema', $field_nutrition_signs values)`.
- **A new query for `family_nutrition_muac_child` measurements**, keyed by their `field_family_nutrition_encounter` parent. Mother MUAC is intentionally excluded — the report is about children.

These measurement bundles are added to `hedley_reports_get_triggering_measurement_types()` so node saves trigger incremental recalcs (the incremental AQ path is still used for all post-deploy live changes).

Internal MUAC unit: cm (the field stores cm; conversion to mm happens only inside the categorization helper).

### Wire format

| Encounter type | Old | New |
|---|---|---|
| Individual nutrition / Well-child / Group nutrition | `"YYYY-MM-DD s,u,w"` | `"YYYY-MM-DD s,u,w,muac,e"` |
| Family nutrition (with MUAC) | `"YYYY-MM-DD"` | `"YYYY-MM-DD muac"` |
| Family nutrition (no MUAC) | `"YYYY-MM-DD"` | `"YYYY-MM-DD"` (unchanged) |

- `muac` is the MUAC value in cm (e.g. `11.8`). Empty string when no MUAC measurement exists for a non-family-nutrition encounter; for family-nutrition encounters with no MUAC, the second token is omitted entirely (date-only form, indistinguishable from the legacy format).
- `e` is `1` if any of the encounter's nutrition-signs values contains `'edema'`, otherwise `0`. Empty string if no `nutrition` / `well_child_nutrition` measurement exists for the encounter at all.

Family nutrition encounters never have edema — there is no `nutrition_signs` field on family nutrition.

### Wire format encoders

`hedley_reports_nutrition_metrics_to_string` is extended to accept `m` and `e` keys and return `"$s,$u,$w,$m,$e"`. A new helper `hedley_reports_family_nutrition_metrics_to_string($values)` returns `$muac` (or empty string).

The per-encounter loop in `hedley_reports_calculate_aggregated_data_for_person`:

- For nutrition / well-child / SPV / group encounters: passes the matching `$muac_edema_by_encounter[$encounter_id]` data into the existing `$encounter_data = "$encounter_date $nutrition"` builder.
- For family nutrition encounters (currently `date("Y-m-d", ...)` only): switches to `"$date $muac"`.

Note: `HEDLEY_STATS_SPV_ENCOUNTER_TYPE` and the `"well-child"` JSON key are the same thing in the codebase; the issue's "SPV encounters" and "well-child encounters" map to the same code path.

## Section 2 — Categorization logic

A new helper, parallel to `hedley_reports_categorize_z_score`:

```php
function hedley_reports_categorize_acute_malnutrition($muac_cm, $has_edema, $person_id) {
  if ($muac_cm === NULL) {
    // No MUAC measurement → encounter does not enter the AM denominator.
    return [[], [], []]; // [normal, mam, sam]
  }
  $muac_mm = $muac_cm * 10;
  if ($muac_mm < 115 || $has_edema) {
    return [[], [], [$person_id]];
  }
  if ($muac_mm < 125) {
    return [[], [$person_id], []];
  }
  return [[$person_id], [], []];
}
```

For family nutrition encounters, `$has_edema` is always `FALSE`. The same helper handles them — SAM falls out as "MUAC < 115 mm" only, exactly per the issue.

The Elm mirror in `Pages/Reports/Utils.elm`:

```elm
categorizeAcuteMalnutrition : PersonId -> Maybe Float -> Bool -> ( List PersonId, List PersonId, List PersonId )
categorizeAcuteMalnutrition personId mMuacCm hasEdema =
    case mMuacCm of
        Nothing ->
            ( [], [], [] )

        Just muacCm ->
            let
                muacMm =
                    muacCm * 10
            in
            if muacMm < 115 || hasEdema then
                ( [], [], [ personId ] )

            else if muacMm < 125 then
                ( [], [ personId ], [] )

            else
                ( [ personId ], [], [] )
```

`hedley_reports_nutrition_encounter_data_to_nutrition_metrics` (PHP) and `nutritionEncounterDataToNutritionMetrics` (Elm) are extended to call this helper alongside the existing z-score categorizations and populate three new buckets in `NutritionMetrics`:

- `acute_malnutrition_normal` / `acuteMalnutritionNormal`
- `acute_malnutrition_mam` / `acuteMalnutritionMam`
- `acute_malnutrition_sam` / `acuteMalnutritionSam`

`hedley_reports_sum_nutrition_metrics` and `sumNutritionMetrics`, plus `hedley_reports_empty_nutrition_metrics` and `emptyNutritionMetrics`, each grow three more lines parallel to the existing keys.

## Section 3 — Prevalence & incidence calculations

### Prevalence

`hedley_reports_generate_prevalence_nutrition_metrics_results` and `generatePrevalenceNutritionMetricsResults` add:

```
amTotal = unique(acuteMalnutritionMam ++ acuteMalnutritionSam ++ acuteMalnutritionNormal)
acuteMalnutritionMam% = |acuteMalnutritionMam| / |amTotal|
acuteMalnutritionSam% = |acuteMalnutritionSam| / |amTotal|
```

The `amTotal` denominator is computed independently from `stuntingTotal` / `wastingTotal` / `underweightTotal`. An encounter with z-scores but no MUAC contributes to existing totals but not to `amTotal`, mirroring how an encounter with no wasting z-score doesn't enter the wasting denominator today.

### Incidence

`hedley_reports_generate_incidence_nutrition_metrics_results` and `generateIncidenceNutritionMetricsResults` add two new rows that mirror `stunting_severe` and `stunting_moderate` exactly, substituting AM buckets:

For SAM:

```
previousPeriodAmMamSam     = unique(prev.acuteMalnutritionMam ++ prev.acuteMalnutritionSam)
previousPeriodAmTotal      = previousPeriodAmMamSam ++ prev.acuteMalnutritionNormal
samTestedInPreviousPeriod  = current.acuteMalnutritionSam ∩ previousPeriodAmTotal
samNotIdentifiedPreviously = current.acuteMalnutritionSam − prev.acuteMalnutritionSam
samIncidence%              = |samTestedInPreviousPeriod ∩ samNotIdentifiedPreviously| / |previousPeriodAmTotal|
```

For MAM, swap "not in `prev.acuteMalnutritionSam`" for "not in `previousPeriodAmMamSam`" — exactly mirroring how `stunting_moderate` differs from `stunting_severe` today.

### Result type

`NutritionMetricsResults` (Elm) and the equivalent PHP array shape grow two new fields:

- `acuteMalnutritionMam` / `acute_malnutrition_mam`
- `acuteMalnutritionSam` / `acute_malnutrition_sam`

`emptyNutritionMetricsResults` adds the two new zero defaults.

No changes are needed to the period-bucketing helpers (`generateMonthlyPrevalenceData`, `generateMonthlyIncidenceData`, `generateQuarterlyIncidenceData`, `generateYearlyIncidenceData`, the data-set resolvers). They thread `NutritionMetrics` opaquely.

## Section 4 — Encounter collection (which encounters enter the tables)

This is where family nutrition is added as a new data source for the 8 tables.

### PHP

`hedley_reports_generate_nutrition_report_data` currently collects encounters from `group_nutrition.{pmtct,fbf,sorwathe,chw,achi}`, `individual.nutrition`, and `individual."well-child"`. It adds:

```php
$family_encounters = [];
if (isset($patient_data->individual->{"family-nutrition"})) {
  foreach ($patient_data->individual->{"family-nutrition"} as $items) {
    $family_encounters = array_merge($family_encounters, $items);
  }
}
$data_for_patient['family_encounters'] = $family_encounters;
```

The existing per-encounter loop parses the new 5-field nutrition format (`s,u,w,muac,e`) and feeds the parsed values into the categorization helpers — both the existing z-score categorizer and the new acute-malnutrition categorizer.

A second loop processes `family_encounters` with the family-nutrition wire format (`"YYYY-MM-DD muac"`), parses `muac`, calls `hedley_reports_categorize_acute_malnutrition($muac, FALSE, $person_id)`, and produces a metrics record where **only** `acute_malnutrition_*` keys are populated; stunting/wasting/underweight remain empty arrays. This metrics record is summed into the same `$encounters_by_month[$key]` bucket the regular nutrition encounters use, via `hedley_reports_sum_nutrition_metrics`.

By construction, family nutrition cannot influence the existing rows: empty z-score arrays sum to empty z-score arrays.

### Elm

`countTotalNutritionEncounters` in `Pages/Reports/Utils.elm` adds `data.familyNutritionData` (since family nutrition now contributes to nutrition stats; affects whether the "no encounters" empty state shows).

A new helper `familyNutritionEncounterToMetrics : PersonId -> FamilyNutritionEncounterData -> NutritionMetrics` parses the new `(date, Maybe muacCm)` shape, calls `categorizeAcuteMalnutrition personId mMuacCm False`, and returns a `NutritionMetrics` with only the `acuteMalnutrition*` keys populated.

The existing `encountersByMonth` builder gets a new branch that folds family nutrition encounters into the same dict via `sumNutritionMetrics` on the relevant month key.

### Decoder updates (`Backend/Reports/Decoder.elm`)

`FamilyNutritionEncounterData` is no longer just `NominalDate`. The type alias becomes:

```elm
type alias FamilyNutritionEncounterData =
    { startDate : NominalDate
    , muacCm : Maybe Float
    }
```

`decodeFamilyNutritionEncounterData` parses `"YYYY-MM-DD"` (legacy) or `"YYYY-MM-DD muac"`. Legacy form returns `muacCm = Nothing` — belt-and-suspenders for the rare race between deploy and recalc completion.

`decodeNutritionEncounterData` similarly accepts 3-field or 5-field comma lists, defaulting `muacCm = Nothing` and `hasEdema = False` for the legacy 3-field form.

`NutritionEncounterData` grows to:

```elm
type alias NutritionEncounterData =
    { startDate : NominalDate
    , nutritionData : Maybe NutritionData
    , muacCm : Maybe Float
    , hasEdema : Bool
    }
```

`nutritionData` keeps its existing optional shape so a measurement-less encounter doesn't break stunting/wasting/underweight, exactly as today.

## Section 5 — Pre-computed table data (large-scope path)

### PHP

`hedley_reports_nutrition_metrics_to_table_data` adds two keys to its output:

```php
$result = [
  'type' => $type,
  'period' => [],
  'stunting_moderate' => [],
  'stunting_severe' => [],
  'wasting_moderate' => [],
  'wasting_severe' => [],
  'underweight_moderate' => [],
  'underweight_severe' => [],
  'acute_malnutrition_mam' => [],
  'acute_malnutrition_sam' => [],
];
```

The per-period loop appends `$values['acute_malnutrition_mam']` and `$values['acute_malnutrition_sam']` (already produced as percentage strings by the prevalence/incidence functions in Section 3).

### Elm decoder (`Backend/Reports/Decoder.elm`)

`decodeBackendGeneratedNutritionReportTableDate` adds:

```elm
|> required "acute_malnutrition_mam" (list string)
|> required "acute_malnutrition_sam" (list string)
```

### Elm model (`Backend/Reports/Model.elm`)

`BackendGeneratedNutritionReportTableDate` grows two fields:

```elm
type alias BackendGeneratedNutritionReportTableDate =
    { tableType : NutritionReportTableType
    , captions : List String
    , stuntingModerate : List String
    , stuntingSevere : List String
    , wastingModerate : List String
    , wastingSevere : List String
    , underweightModerate : List String
    , underweightSevere : List String
    , acuteMalnutritionMam : List String
    , acuteMalnutritionSam : List String
    }
```

These fields are `required`, not `optional`. By the time the new Elm code is shipped, the deploy procedure (Section 7) guarantees these keys exist on every `report_data` node — a missing key indicates a deploy-ordering bug we want to fail loudly on. This matches how the existing `stunting_moderate` etc. are already `required`.

## Section 6 — View rendering & translations

### View

`backendGeneratedNutritionReportTableDateToMetricsResultsTableData` in `Pages/Reports/Utils.elm` adds two rows to its `body` list:

```elm
[ translate language Translate.StuntingModerate     :: backendTableData.stuntingModerate
, translate language Translate.StuntingSevere       :: backendTableData.stuntingSevere
, translate language Translate.WastingModerate      :: backendTableData.wastingModerate
, translate language Translate.WastingSevere        :: backendTableData.wastingSevere
, translate language Translate.UnderweightModerate  :: backendTableData.underweightModerate
, translate language Translate.UnderweightSevere    :: backendTableData.underweightSevere
, translate language Translate.AcuteMalnutritionMam :: backendTableData.acuteMalnutritionMam
, translate language Translate.AcuteMalnutritionSam :: backendTableData.acuteMalnutritionSam
]
```

The frontend-computed path uses `generareNutritionReportDataFromRawData` → `generateRow` calls in `Pages/Reports/Utils.elm`. It gets two new `generateRow` calls fed by `.acuteMalnutritionMam` / `.acuteMalnutritionSam` from `NutritionMetricsResults`.

Both rendering paths produce identical row structures, so no CSS / table-layout changes are needed.

### Translations (`server/elm/src/Translate.elm`)

Two new `TranslationId` variants, alphabetized into the existing `type TranslationId`:

- `AcuteMalnutritionMam`
- `AcuteMalnutritionSam`

And two new entries in `translationSet`, alphabetized:

```elm
AcuteMalnutritionMam ->
    { english = "MAM (Moderate Acute Malnutrition)"
    , kinyarwanda = Nothing
    , kirundi = Nothing
    , somali = Nothing
    }

AcuteMalnutritionSam ->
    { english = "SAM (Severe Acute Malnutrition)"
    , kinyarwanda = Nothing
    , kirundi = Nothing
    , somali = Nothing
    }
```

Per the project's `CLAUDE.md` convention, the non-English fields stay `Nothing` and fall back to English at runtime until translators provide localized text.

## Section 7 — Deployment, backfill & rollout

The wire-format change means existing `field_reports_data` blobs and existing `report_data` cached nodes need to be regenerated against the new code. We use the existing recalc scripts — no new files needed.

**Deploy procedure (single maintenance window):**

1. Deploy backend + frontend code together (PHP report generator and Elm decoder are version-locked by the wire format).
2. `drush scr profiles/hedley/modules/custom/hedley_reports/scripts/generate-data-for-all.php` — recalculates `field_reports_data` for every published person on the new wire format. Resume with `--nid=<last>` if interrupted. Run **without** `--exclude_set` so existing data is overwritten with the new format.
3. `drush scr profiles/hedley/modules/custom/hedley_reports/scripts/recalculate-large-datasets.php` — refreshes the pre-computed `additional.nutrition_report_data` for global / province / district / health-center scopes. Reads from the now-updated person data.
4. Done.

Both scripts already exist, already handle batching (`--batch`), memory ceilings (`--memory_limit`), and resume (`--nid`). Step 2 is the long one; its wall-clock time scales linearly with the number of persons.

**Why not Advanced Queue?** AQ defers work via cron-driven workers; for a one-shot bulk recalc on a known dataset, the queue overhead and the throttling hurt and give nothing in return. The point of the recalc is to be done before users hit the report — synchronous direct recalc is the right tool, and `generate-data-for-all.php` already implements it.

**Decoder tolerance:** the legacy-format tolerance in `decodeNutritionEncounterData` and `decodeFamilyNutritionEncounterData` (Section 4) is belt-and-suspenders, not load-bearing. It guards against the rare race where a person node is created mid-deploy, between deploy completion and step 2 starting. The `BackendGeneratedNutritionReportTableDate` decoder remains `required` because the large-scope `report_data` recalc in step 3 is the strict gate — there is no race to tolerate there.

**Tests:** there are no existing PHP tests for `hedley_reports_generate_nutrition_report_data` or its helpers. This change does not introduce new automated tests; the existing parallel structure between PHP and Elm pipelines is the regression safety. Manual verification post-deploy:

- Open a sector / cell / village statistical query and confirm the 8 tables now show 8 rows each, with MAM and SAM populated for periods that have MUAC data.
- Open a global / province / district / health-center statistical query and confirm the same.
- Verify that an encounter with z-scores but no MUAC still appears in the existing rows but does not affect MAM/SAM denominators.
- Verify that family nutrition encounters with MUAC contribute to MAM/SAM but not to stunting/wasting/underweight.

## File touch list

**PHP (`server/hedley/modules/custom/hedley_reports/hedley_reports.module`)** — single file:

- `hedley_reports_get_triggering_measurement_types` — add MUAC and nutrition-signs bundles to incremental-recalc triggers.
- `hedley_reports_calculate_aggregated_data_for_person` — build `$muac_edema_by_encounter` map from MUAC + nutrition-signs measurements; query `family_nutrition_muac_child` measurements; emit new wire format for nutrition / well-child / group / family-nutrition encounters.
- `hedley_reports_nutrition_metrics_to_string` — extend to encode `s,u,w,muac,e`.
- New helper `hedley_reports_family_nutrition_metrics_to_string` (or fold into the existing helper with a flag).
- `hedley_reports_generate_nutrition_report_data` — collect family-nutrition encounters; parse new 5-field format; second loop for family encounters producing AM-only metrics.
- New helper `hedley_reports_categorize_acute_malnutrition`.
- `hedley_reports_nutrition_encounter_data_to_nutrition_metrics` — add AM categorization call, return three new buckets.
- `hedley_reports_sum_nutrition_metrics` — three new merge lines.
- `hedley_reports_empty_nutrition_metrics` — three new keys.
- `hedley_reports_generate_prevalence_nutrition_metrics_results` — `amTotal` denominator + two new percentages.
- `hedley_reports_generate_incidence_nutrition_metrics_results` — two new incidence calculations mirroring stunting_severe / stunting_moderate.
- `hedley_reports_nutrition_metrics_to_table_data` — two new output keys.

**Elm — admin reports app (`server/elm/src/`)**:

- `Backend/Reports/Model.elm` — extend `NutritionEncounterData`, `FamilyNutritionEncounterData`, `NutritionMetrics` (3 new), `NutritionMetricsResults` (2 new), `BackendGeneratedNutritionReportTableDate` (2 new), `emptyNutritionMetrics`, `emptyNutritionMetricsResults`.
- `Backend/Reports/Decoder.elm` — `decodeNutritionEncounterData` accepts 3-field or 5-field; `decodeFamilyNutritionEncounterData` accepts date-only or date+muac; `decodeBackendGeneratedNutritionReportTableDate` adds two `required` lines.
- `Pages/Reports/Utils.elm` — `categorizeAcuteMalnutrition`, `nutritionEncounterDataToNutritionMetrics` extension, `familyNutritionEncounterToMetrics`, `sumNutritionMetrics` extension, `countTotalNutritionEncounters` includes family nutrition, `generatePrevalenceNutritionMetricsResults` extension, `generateIncidenceNutritionMetricsResults` extension, `backendGeneratedNutritionReportTableDateToMetricsResultsTableData` adds two rows, raw-data `generateRow` calls add two rows.
- `Translate.elm` — two new `TranslationId` variants and translation set entries (alphabetized).

**No new files** are introduced. Both recalc scripts (`generate-data-for-all.php` and `recalculate-large-datasets.php`) already exist.

## Out of scope

- Renaming the misleading `'w'`/`'u'` PHP variable naming in `hedley_reports_calculate_aggregated_data_for_person` and friends. Pre-existing confusion; touching it would widen the diff with no scope-relevant benefit.
- Adding PHP unit tests for nutrition report calculations. None exist today; introducing them is a separate effort.
- Localized translations for Kinyarwanda / Kirundi / Somali. The new strings are added in English only; translators provide the rest later.
- Including MUAC / edema in any non-statistical-queries report (e.g. progress reports, dashboards).

## Open questions

None outstanding — all clarifying questions in the brainstorming session were resolved.
