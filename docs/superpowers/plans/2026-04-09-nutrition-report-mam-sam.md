# MAM/SAM Rows in Nutrition Statistical Queries — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add MAM (Moderate Acute Malnutrition) and SAM (Severe Acute Malnutrition) rows to all 8 nutrition tables in the Statistical Queries report, computed from MUAC and edema (not z-scores), in both the PHP large-scope and Elm small-scope code paths.

**Architecture:** Symmetrical extension of the existing nutrition report pipeline. Two new buckets (`acute_malnutrition_mam` / `acute_malnutrition_sam`) are added everywhere stunting/wasting/underweight already exist — sum, empty, prevalence, incidence, table data, view rows. The wire format stored in `field_reports_data` is extended to carry MUAC + edema per encounter. Family nutrition encounters become a new data source contributing to MAM/SAM only.

**Tech Stack:** PHP 7 (Drupal 7 / Hedley install profile), Elm 0.19.1 (admin-side app at `server/elm/`).

**Spec:** `docs/superpowers/specs/2026-04-09-nutrition-report-mam-sam-design.md`

**Branch:** `issue-1718-mam-sam` (already created, branched off `develop`).

**Issue:** [TIP-Global-Health/eheza-app#1718](https://github.com/TIP-Global-Health/eheza-app/issues/1718)

---

## Important constraints

These come from the spec, project conventions (`CLAUDE.md`), and persisted user preferences. **Read them before starting any task.**

- **No new automated tests.** The spec explicitly states no PHP unit tests exist for the nutrition report helpers and none are added in this PR. Verification is via lint, manual smoke testing, and the parallel PHP↔Elm structure as regression safety. Do not invent tests.
- **Ask before compiling Elm.** The user wants to be asked before any `elm make`, `gulp`, or `gulp publish` invocation. `elm-format --validate` and `elm-format` are fine without asking. If a task says "compile and verify", surface that to the user instead of running it directly.
- **No `Debug.log` in committed Elm.** Strip any debugging output before staging.
- **Alphabetical ordering in Elm.** Union type variants, case branches, and pattern matches must stay alphabetical. New `TranslationId` variants get inserted in the alphabetical position, not appended at the end.
- **Translations: leave non-English fields as `Nothing`.** Do not write `Just "MAM"` etc. — runtime falls back to English.
- **Do not commit `.ddev/.gitignore`.** It is currently dirty in the working tree and must be excluded from every commit. Stage files explicitly, never use `git add -A` / `git add .`.
- **Append `[ci skip]` to every commit message** (after `Co-Authored-By`), unless the user explicitly asks for CI to run.
- **Never amend a commit.** Always create a new commit. If a hook fails, fix the issue, re-stage, and create a new commit.

---

## File touch map

**PHP — single file:** `server/hedley/modules/custom/hedley_reports/hedley_reports.module`

| Function | Change |
|---|---|
| `hedley_reports_get_triggering_measurement_types` | Add `HEDLEY_ACTIVITY_MUAC_BUNDLES` and `HEDLEY_ACTIVITY_NUTRITION_BUNDLES` to the merge. |
| `hedley_reports_calculate_aggregated_data_for_person` | Build `$muac_edema_by_encounter` map in the measurements switch (5 new cases); query `family_nutrition_muac_child` measurements; emit new wire format for nutrition / well-child / group / family-nutrition encounters. |
| `hedley_reports_nutrition_metrics_to_string` | Extend to encode `s,u,w,muac,e`. |
| New: `hedley_reports_family_nutrition_metrics_to_string` | Returns `$muac` or `''`. |
| New: `hedley_reports_categorize_acute_malnutrition` | MUAC + edema → `[normal, mam, sam]` arrays. |
| `hedley_reports_empty_nutrition_metrics` | Add 3 new keys. |
| `hedley_reports_sum_nutrition_metrics` | Merge 3 new keys. |
| `hedley_reports_nutrition_encounter_data_to_nutrition_metrics` | Take new MUAC/edema params; call new categorizer; return 3 new buckets. |
| `hedley_reports_generate_prevalence_nutrition_metrics_results` | Add `am_total` denominator + 2 new percentages. |
| `hedley_reports_generate_incidence_nutrition_metrics_results` | Add 2 new incidence calculations mirroring `stunting_severe`/`stunting_moderate`. |
| `hedley_reports_nutrition_metrics_to_table_data` | Add 2 new output keys + per-period appends. |
| `hedley_reports_generate_nutrition_report_data` | Collect family-nutrition encounters; parse new 5-field format; second loop for family encounters producing AM-only metrics. |

**Elm — admin reports app (`server/elm/src/`):**

| File | Change |
|---|---|
| `Backend/Reports/Model.elm` | Extend `NutritionEncounterData`, `FamilyNutritionEncounterData`, `BackendGeneratedNutritionReportTableDate`. |
| `Backend/Reports/Decoder.elm` | Tolerant decoders for `NutritionEncounterData` and `FamilyNutritionEncounterData`; required new fields on `BackendGeneratedNutritionReportTableDate`. |
| `Pages/Reports/Model.elm` | Extend `NutritionMetrics` (3 new), `NutritionMetricsResults` (2 new), `emptyNutritionMetrics`, `emptyNutritionMetricsResults`. |
| `Pages/Reports/Utils.elm` | New `categorizeAcuteMalnutrition`, `familyNutritionEncounterToMetrics`. Extend `nutritionEncounterDataToNutritionMetrics`, `sumNutritionMetrics`, `countTotalNutritionEncounters`, `generatePrevalenceNutritionMetricsResults`, `generateIncidenceNutritionMetricsResults`, `generareNutritionReportDataFromRawData`'s `generateRow` calls, `backendGeneratedNutritionReportTableDateToMetricsResultsTableData`'s body list. Integrate family nutrition into the encounters-by-month builder. |
| `Translate.elm` | New `AcuteMalnutritionMam`, `AcuteMalnutritionSam` `TranslationId` variants and translation set entries (alphabetized). |

**No new files.** Both recalc scripts (`generate-data-for-all.php`, `recalculate-large-datasets.php`) already exist.

---

## Commit order rationale

The wire-format change couples PHP writers and Elm decoders. We sequence so that every commit compiles and the work-in-progress branch is never in a broken intermediate state at any point a reviewer might check it out.

1. **Task 1 — Elm wire-format types + tolerant decoders.** Adds the new type fields (`muacCm`, `hasEdema`) populated as `Nothing` / `False` from the legacy 3-field nutrition format and date-only family format. Compiles. No behavior change because nothing reads the new fields yet.
2. **Task 2 — Elm metric primitives.** New buckets in `NutritionMetrics`, new helpers (`categorizeAcuteMalnutrition`, `familyNutritionEncounterToMetrics`), `sumNutritionMetrics` extension, `nutritionEncounterDataToNutritionMetrics` extension. Pure additions; existing rows unaffected.
3. **Task 3 — Elm small-scope path completion.** `NutritionMetricsResults` extension, prevalence + incidence extensions, family-encounter integration into encounters-by-month builder, `countTotalNutritionEncounters` extension, `generareNutritionReportDataFromRawData` view rows, `Translate.elm` strings. Sector / cell / village reports now show the new rows (still empty until PHP starts populating MUAC/edema).
4. **Task 4 — PHP wire-format writers.** PHP starts emitting the new 5-field nutrition format and family-nutrition `date muac` format. Tolerant decoders from Task 1 absorb this gracefully. Triggering measurement bundles updated.
5. **Task 5 — PHP nutrition report computation.** Categorization helper, empty/sum/encounter→metrics extensions, prevalence/incidence/table_data extensions, family-encounter loop in `generate_nutrition_report_data`. PHP-precomputed JSON now contains `acute_malnutrition_*` keys.
6. **Task 6 — Elm large-scope path completion.** `BackendGeneratedNutritionReportTableDate` extension (required new fields), decoder, `backendGeneratedNutritionReportTableDateToMetricsResultsTableData` view rows. Global / province / district / health-center reports now show the new rows.
7. **Task 7 — Lint pass.**
8. **Task 8 — Local recalc + manual smoke test.** Drush scripts to regenerate `field_reports_data` and `report_data` against the new code, then manual UI verification.

---

## Task 1 — Elm wire-format types and tolerant decoders

**Files:**
- Modify: `server/elm/src/Backend/Reports/Model.elm` (around lines 196–229: `NutritionEncounterData`, `NutritionData`, `FamilyNutritionEncounterData`)
- Modify: `server/elm/src/Backend/Reports/Decoder.elm` (around lines 508–548: `decodeFamilyNutritionEncounterData`, `decodeNutritionEncounterData`, `nutritionDataFromString`)

### Step 1.1 — Read the existing types

- [ ] Open `server/elm/src/Backend/Reports/Model.elm`. Locate the `NutritionEncounterData`, `NutritionData`, and `FamilyNutritionEncounterData` definitions (around lines 196–229). Confirm they currently look like:

```elm
type alias NutritionEncounterData =
    { startDate : NominalDate
    , nutritionData : Maybe NutritionData
    }


type alias NutritionData =
    { stunting : Maybe Float
    , wasting : Maybe Float
    , underweight : Maybe Float
    }

type alias FamilyNutritionEncounterData =
    NominalDate
```

### Step 1.2 — Extend `NutritionEncounterData`

- [ ] Replace the `NutritionEncounterData` type alias with:

```elm
type alias NutritionEncounterData =
    { startDate : NominalDate
    , nutritionData : Maybe NutritionData
    , muacCm : Maybe Float
    , hasEdema : Bool
    }
```

### Step 1.3 — Extend `FamilyNutritionEncounterData`

- [ ] Replace the `FamilyNutritionEncounterData` type alias with:

```elm
type alias FamilyNutritionEncounterData =
    { startDate : NominalDate
    , muacCm : Maybe Float
    }
```

### Step 1.4 — Update `decodeNutritionEncounterData` to be tolerant of both formats

- [ ] Open `server/elm/src/Backend/Reports/Decoder.elm`. Locate `decodeNutritionEncounterData` (around line 513). Replace it with:

```elm
decodeNutritionEncounterData : Decoder NutritionEncounterData
decodeNutritionEncounterData =
    string
        |> andThen
            (\s ->
                case String.split " " (String.trim s) of
                    [ first ] ->
                        Date.fromIsoString first
                            |> Result.toMaybe
                            |> Maybe.map
                                (\startDate ->
                                    succeed
                                        { startDate = startDate
                                        , nutritionData = Nothing
                                        , muacCm = Nothing
                                        , hasEdema = False
                                        }
                                )
                            |> Maybe.withDefault (fail "Failed to decode NutritionEncounterData")

                    [ first, second ] ->
                        Date.fromIsoString first
                            |> Result.toMaybe
                            |> Maybe.map
                                (\startDate ->
                                    let
                                        ( nutritionData, muacCm, hasEdema ) =
                                            parseNutritionEncounterPayload second
                                    in
                                    succeed
                                        { startDate = startDate
                                        , nutritionData = nutritionData
                                        , muacCm = muacCm
                                        , hasEdema = hasEdema
                                        }
                                )
                            |> Maybe.withDefault (fail "Failed to decode NutritionEncounterData")

                    _ ->
                        fail "Failed to decode NutritionEncounterData"
            )
```

### Step 1.5 — Replace `nutritionDataFromString` with `parseNutritionEncounterPayload`

- [ ] Replace the existing `nutritionDataFromString` helper with a new helper that handles both legacy 3-field and new 5-field forms:

```elm
parseNutritionEncounterPayload : String -> ( Maybe NutritionData, Maybe Float, Bool )
parseNutritionEncounterPayload payload =
    case String.split "," payload of
        [ stunting, underweight, wasting ] ->
            ( Just (NutritionData (String.toFloat stunting) (String.toFloat wasting) (String.toFloat underweight))
            , Nothing
            , False
            )

        [ stunting, underweight, wasting, muac, edema ] ->
            ( Just (NutritionData (String.toFloat stunting) (String.toFloat wasting) (String.toFloat underweight))
            , String.toFloat muac
            , edema == "1"
            )

        _ ->
            ( Nothing, Nothing, False )
```

Note the wire format is `s,u,w` (stunting, underweight, wasting) — not `s,w,u`. The constructor argument order is `NutritionData stunting wasting underweight`, which is why the destructured `wasting` from the comma list goes into the second constructor slot. This mirrors the existing legacy logic.

### Step 1.6 — Update `decodeFamilyNutritionEncounterData` to be tolerant of both formats

- [ ] Replace `decodeFamilyNutritionEncounterData` with:

```elm
decodeFamilyNutritionEncounterData : Decoder FamilyNutritionEncounterData
decodeFamilyNutritionEncounterData =
    string
        |> andThen
            (\s ->
                case String.split " " (String.trim s) of
                    [ first ] ->
                        Date.fromIsoString first
                            |> Result.toMaybe
                            |> Maybe.map
                                (\startDate ->
                                    succeed { startDate = startDate, muacCm = Nothing }
                                )
                            |> Maybe.withDefault (fail "Failed to decode FamilyNutritionEncounterData")

                    [ first, second ] ->
                        Date.fromIsoString first
                            |> Result.toMaybe
                            |> Maybe.map
                                (\startDate ->
                                    succeed { startDate = startDate, muacCm = String.toFloat second }
                                )
                            |> Maybe.withDefault (fail "Failed to decode FamilyNutritionEncounterData")

                    _ ->
                        fail "Failed to decode FamilyNutritionEncounterData"
            )
```

### Step 1.7 — Remove `decodeYYYYMMDD` import if it's now unused

- [ ] At the top of `server/elm/src/Backend/Reports/Decoder.elm`, check if `decodeYYYYMMDD` is still used elsewhere in the file. If not, remove its import. If still used (likely yes — other encounter types use it), leave it.

### Step 1.8 — Format the file

- [ ] Run from `/var/www/html/sec-ihangane`:

```bash
elm-format server/elm/src/Backend/Reports/Model.elm server/elm/src/Backend/Reports/Decoder.elm
```

Expected: no errors. Files are reformatted in place if needed.

### Step 1.9 — Verify compilation

- [ ] **Ask the user** to compile the admin Elm app to confirm both files type-check. (Per user preference: do not run `elm make` / `gulp` without asking first.) Wait for confirmation that compilation succeeds before continuing.

### Step 1.10 — Inspect dependent call sites

- [ ] Run `Grep` for usages of `FamilyNutritionEncounterData` across `server/elm/src/`. Anything that previously treated it as a `NominalDate` (e.g. `Date` operations directly on the value) needs adjustment to access `.startDate`. Common locations to check:
  - `Pages/Reports/Utils.elm` — may use family nutrition dates in encounter folding.
  - `Pages/Reports/View.elm` — may use family nutrition for summary counts.

If any call site does `someFamilyEncounterValue` as if it were a `Date`, change it to `someFamilyEncounterValue.startDate`. **Do not rewrite logic — only adjust field access.** If unsure whether a change is purely mechanical, surface it to the user.

### Step 1.11 — Verify compilation again after adjustments

- [ ] If you made adjustments in Step 1.10, ask the user to compile again.

### Step 1.12 — Commit

- [ ] Run:

```bash
git status
```

Expected: only the two modified Elm files in the staging candidates (no `.ddev/.gitignore`).

- [ ] Run:

```bash
git add server/elm/src/Backend/Reports/Model.elm server/elm/src/Backend/Reports/Decoder.elm
```

If Step 1.10 modified additional Elm files, add them too — by name, never with `-A`/`.`.

- [ ] Commit:

```bash
git commit -m "$(cat <<'EOF'
Issue #1718: extend Elm reports types for MUAC and edema

Add muacCm and hasEdema to NutritionEncounterData and muacCm to
FamilyNutritionEncounterData. Decoders accept both the legacy formats
and the upcoming 5-field nutrition / date+muac family-nutrition forms,
defaulting new fields to Nothing/False so existing data continues to
work unchanged.

Refs #1718

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

- [ ] Run `git status` and confirm the commit landed and the working tree is back to its prior state (only `.ddev/.gitignore` remaining as a pre-existing modification).

---

## Task 2 — Elm metric primitives

Adds the new `acuteMalnutrition*` buckets to `NutritionMetrics`, the new helpers (`categorizeAcuteMalnutrition`, `familyNutritionEncounterToMetrics`), and extends `sumNutritionMetrics` and `nutritionEncounterDataToNutritionMetrics`. Pure additions — no existing behavior changes.

**Files:**
- Modify: `server/elm/src/Pages/Reports/Model.elm` (around lines 45–69: `NutritionMetrics` and `emptyNutritionMetrics`)
- Modify: `server/elm/src/Pages/Reports/Utils.elm` (around lines 109–168: `sumNutritionMetrics`, `nutritionEncounterDataToNutritionMetrics`)

### Step 2.1 — Extend `NutritionMetrics`

- [ ] Open `server/elm/src/Pages/Reports/Model.elm`. Locate `NutritionMetrics` (around line 45). Replace it with:

```elm
type alias NutritionMetrics =
    { stuntingNormal : List PersonId
    , stuntingModerate : List PersonId
    , stuntingSevere : List PersonId
    , wastingNormal : List PersonId
    , wastingModerate : List PersonId
    , wastingSevere : List PersonId
    , underweightNormal : List PersonId
    , underweightModerate : List PersonId
    , underweightSevere : List PersonId
    , acuteMalnutritionNormal : List PersonId
    , acuteMalnutritionMam : List PersonId
    , acuteMalnutritionSam : List PersonId
    }
```

### Step 2.2 — Extend `emptyNutritionMetrics`

- [ ] Replace `emptyNutritionMetrics` with:

```elm
emptyNutritionMetrics : NutritionMetrics
emptyNutritionMetrics =
    { stuntingNormal = []
    , stuntingModerate = []
    , stuntingSevere = []
    , wastingNormal = []
    , wastingModerate = []
    , wastingSevere = []
    , underweightNormal = []
    , underweightModerate = []
    , underweightSevere = []
    , acuteMalnutritionNormal = []
    , acuteMalnutritionMam = []
    , acuteMalnutritionSam = []
    }
```

### Step 2.3 — Extend `sumNutritionMetrics`

- [ ] Open `server/elm/src/Pages/Reports/Utils.elm`. Locate `sumNutritionMetrics` (around line 109). Replace it with:

```elm
sumNutritionMetrics : List NutritionMetrics -> NutritionMetrics
sumNutritionMetrics =
    List.foldl
        (\metrics accum ->
            { accum
                | stuntingNormal = accum.stuntingNormal ++ metrics.stuntingNormal
                , stuntingModerate = accum.stuntingModerate ++ metrics.stuntingModerate
                , stuntingSevere = accum.stuntingSevere ++ metrics.stuntingSevere
                , wastingNormal = accum.wastingNormal ++ metrics.wastingNormal
                , wastingModerate = accum.wastingModerate ++ metrics.wastingModerate
                , wastingSevere = accum.wastingSevere ++ metrics.wastingSevere
                , underweightNormal = accum.underweightNormal ++ metrics.underweightNormal
                , underweightModerate = accum.underweightModerate ++ metrics.underweightModerate
                , underweightSevere = accum.underweightSevere ++ metrics.underweightSevere
                , acuteMalnutritionNormal = accum.acuteMalnutritionNormal ++ metrics.acuteMalnutritionNormal
                , acuteMalnutritionMam = accum.acuteMalnutritionMam ++ metrics.acuteMalnutritionMam
                , acuteMalnutritionSam = accum.acuteMalnutritionSam ++ metrics.acuteMalnutritionSam
            }
        )
        emptyNutritionMetrics
```

### Step 2.4 — Add `categorizeAcuteMalnutrition` helper

- [ ] In `server/elm/src/Pages/Reports/Utils.elm`, immediately above `nutritionEncounterDataToNutritionMetrics`, insert:

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

The return tuple is `(normal, mam, sam)`. Mirrors the categorize-z-score pattern used inline today.

### Step 2.5 — Extend `nutritionEncounterDataToNutritionMetrics`

- [ ] Replace `nutritionEncounterDataToNutritionMetrics` with:

```elm
nutritionEncounterDataToNutritionMetrics : PersonId -> NutritionEncounterData -> NutritionMetrics
nutritionEncounterDataToNutritionMetrics personId encounter =
    let
        categorizeZScore =
            Maybe.map
                (\score ->
                    if score <= -3 then
                        ( [], [], [ personId ] )

                    else if score <= -2 then
                        ( [], [ personId ], [] )

                    else
                        ( [ personId ], [], [] )
                )
                >> Maybe.withDefault ( [], [], [] )

        ( stuntingNormal, stuntingModerate, stuntingSevere ) =
            encounter.nutritionData
                |> Maybe.map (.stunting >> categorizeZScore)
                |> Maybe.withDefault ( [], [], [] )

        ( wastingNormal, wastingModerate, wastingSevere ) =
            encounter.nutritionData
                |> Maybe.map (.wasting >> categorizeZScore)
                |> Maybe.withDefault ( [], [], [] )

        ( underweightNormal, underweightModerate, underweightSevere ) =
            encounter.nutritionData
                |> Maybe.map (.underweight >> categorizeZScore)
                |> Maybe.withDefault ( [], [], [] )

        ( acuteMalnutritionNormal, acuteMalnutritionMam, acuteMalnutritionSam ) =
            categorizeAcuteMalnutrition personId encounter.muacCm encounter.hasEdema
    in
    { stuntingNormal = stuntingNormal
    , stuntingModerate = stuntingModerate
    , stuntingSevere = stuntingSevere
    , wastingNormal = wastingNormal
    , wastingModerate = wastingModerate
    , wastingSevere = wastingSevere
    , underweightNormal = underweightNormal
    , underweightModerate = underweightModerate
    , underweightSevere = underweightSevere
    , acuteMalnutritionNormal = acuteMalnutritionNormal
    , acuteMalnutritionMam = acuteMalnutritionMam
    , acuteMalnutritionSam = acuteMalnutritionSam
    }
```

This rewrite changes the function from operating on `.nutritionData` directly to operating on the whole `NutritionEncounterData` record so that `.muacCm` and `.hasEdema` are also reachable. The behavior for the existing rows is unchanged: an encounter with no `nutritionData` still yields empty z-score buckets.

### Step 2.6 — Add `familyNutritionEncounterToMetrics` helper

- [ ] Immediately below `nutritionEncounterDataToNutritionMetrics`, insert:

```elm
familyNutritionEncounterToMetrics : PersonId -> FamilyNutritionEncounterData -> NutritionMetrics
familyNutritionEncounterToMetrics personId encounter =
    let
        ( acuteMalnutritionNormal, acuteMalnutritionMam, acuteMalnutritionSam ) =
            categorizeAcuteMalnutrition personId encounter.muacCm False
    in
    { emptyNutritionMetrics
        | acuteMalnutritionNormal = acuteMalnutritionNormal
        , acuteMalnutritionMam = acuteMalnutritionMam
        , acuteMalnutritionSam = acuteMalnutritionSam
    }
```

Family nutrition has no edema data, so `hasEdema` is always `False`. Z-score buckets stay empty (inherited from `emptyNutritionMetrics`), guaranteeing family nutrition cannot influence the existing rows.

### Step 2.7 — Format

- [ ] Run from `/var/www/html/sec-ihangane`:

```bash
elm-format server/elm/src/Pages/Reports/Model.elm server/elm/src/Pages/Reports/Utils.elm
```

### Step 2.8 — Verify compilation

- [ ] **Ask the user** to compile and confirm both files type-check. (Existing call sites of `nutritionEncounterDataToNutritionMetrics` only pass a `PersonId` curried — the new version still has the same outer signature so it should still work, but Step 2.5 changed the inner shape from `>>` to direct application. Surface any compile error to the user.)

### Step 2.9 — Commit

- [ ] Run:

```bash
git add server/elm/src/Pages/Reports/Model.elm server/elm/src/Pages/Reports/Utils.elm
git commit -m "$(cat <<'EOF'
Issue #1718: add acute malnutrition buckets to Elm metric primitives

Extends NutritionMetrics with acuteMalnutritionNormal/Mam/Sam, adds
categorizeAcuteMalnutrition (MUAC + edema → buckets) and a family-
nutrition encounter-to-metrics helper that populates only the new
buckets. sumNutritionMetrics and nutritionEncounterDataToNutritionMetrics
extended to thread the new fields. No callers consume them yet.

Refs #1718

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

---

## Task 3 — Elm small-scope path completion (prevalence, incidence, view rows, translations)

Completes the Elm side of the small-scope (sector / cell / village) report. After this commit, those reports will render the two new rows even before any PHP changes — the rows will simply show 0% until PHP starts emitting MUAC/edema in Task 4.

**Files:**
- Modify: `server/elm/src/Pages/Reports/Model.elm` (around lines 72–90: `NutritionMetricsResults`, `emptyNutritionMetricsResults`)
- Modify: `server/elm/src/Pages/Reports/Utils.elm` (multiple functions: `countTotalNutritionEncounters`, `generatePrevalenceNutritionMetricsResults`, `generateIncidenceNutritionMetricsResults`, the encounters-by-month builder, `generareNutritionReportDataFromRawData`'s row generators)
- Modify: `server/elm/src/Translate.elm`

### Step 3.1 — Extend `NutritionMetricsResults`

- [ ] In `server/elm/src/Pages/Reports/Model.elm`, replace `NutritionMetricsResults` with:

```elm
type alias NutritionMetricsResults =
    { stuntingModerate : Float
    , stuntingSevere : Float
    , wastingModerate : Float
    , wastingSevere : Float
    , underweightModerate : Float
    , underweightSevere : Float
    , acuteMalnutritionMam : Float
    , acuteMalnutritionSam : Float
    }
```

### Step 3.2 — Extend `emptyNutritionMetricsResults`

- [ ] Replace `emptyNutritionMetricsResults` with:

```elm
emptyNutritionMetricsResults : NutritionMetricsResults
emptyNutritionMetricsResults =
    { stuntingModerate = 0
    , stuntingSevere = 0
    , wastingModerate = 0
    , wastingSevere = 0
    , underweightModerate = 0
    , underweightSevere = 0
    , acuteMalnutritionMam = 0
    , acuteMalnutritionSam = 0
    }
```

### Step 3.3 — Extend `countTotalNutritionEncounters`

- [ ] Open `server/elm/src/Pages/Reports/Utils.elm`. Locate `countTotalNutritionEncounters` (around line 87). Replace its body to also include family nutrition:

```elm
countTotalNutritionEncounters : PatientData -> Int
countTotalNutritionEncounters data =
    let
        countGroupDataEncounters =
            Maybe.map List.length
                >> Maybe.withDefault 0
    in
    countIndividualDataEncounters data.wellChildData
        + countIndividualDataEncounters data.individualNutritionData
        + countIndividualDataEncounters data.familyNutritionData
        + countGroupDataEncounters data.groupNutritionPmtctData
        + countGroupDataEncounters data.groupNutritionFbfData
        + countGroupDataEncounters data.groupNutritionSorwatheData
        + countGroupDataEncounters data.groupNutritionChwData
        + countGroupDataEncounters data.groupNutritionAchiData
```

(Verify the field name is `familyNutritionData` by reading `Backend/Reports/Model.elm`'s `PatientData` definition before applying — it should be `Maybe (List (List FamilyNutritionEncounterData))`.)

### Step 3.4 — Extend `generatePrevalenceNutritionMetricsResults`

- [ ] Locate `generatePrevalenceNutritionMetricsResults` (around line 171). Replace it with:

```elm
generatePrevalenceNutritionMetricsResults : NutritionMetrics -> NutritionMetricsResults
generatePrevalenceNutritionMetricsResults metrics =
    let
        calculatePercentage nominator total =
            if List.isEmpty total then
                0

            else
                (toFloat (List.length nominator) / toFloat (List.length total)) * 100

        stuntingTotal =
            metrics.stuntingModerate
                ++ metrics.stuntingSevere
                ++ metrics.stuntingNormal
                |> unique

        wastingTotal =
            metrics.wastingModerate
                ++ metrics.wastingSevere
                ++ metrics.wastingNormal
                |> unique

        underweightTotal =
            metrics.underweightModerate
                ++ metrics.underweightSevere
                ++ metrics.underweightNormal
                |> unique

        acuteMalnutritionTotal =
            metrics.acuteMalnutritionMam
                ++ metrics.acuteMalnutritionSam
                ++ metrics.acuteMalnutritionNormal
                |> unique
    in
    { stuntingModerate = calculatePercentage metrics.stuntingModerate stuntingTotal
    , stuntingSevere = calculatePercentage metrics.stuntingSevere stuntingTotal
    , wastingModerate = calculatePercentage metrics.wastingModerate wastingTotal
    , wastingSevere = calculatePercentage metrics.wastingSevere wastingTotal
    , underweightModerate = calculatePercentage metrics.underweightModerate underweightTotal
    , underweightSevere = calculatePercentage metrics.underweightSevere underweightTotal
    , acuteMalnutritionMam = calculatePercentage metrics.acuteMalnutritionMam acuteMalnutritionTotal
    , acuteMalnutritionSam = calculatePercentage metrics.acuteMalnutritionSam acuteMalnutritionTotal
    }
```

### Step 3.5 — Extend `generateIncidenceNutritionMetricsResults`

- [ ] Locate `generateIncidenceNutritionMetricsResults` (around line 208). Read the existing function fully — it has stunting, wasting, and underweight blocks. Add a fourth, parallel block for acute malnutrition. Then return a record with the two new fields.

Add inside the `let` block, after the existing UNDERWEIGHT block:

```elm
        -- ACUTE MALNUTRITION
        previousPeriodAcuteMalnutritionMamSam =
            previousPeriodMetric.acuteMalnutritionMam
                ++ previousPeriodMetric.acuteMalnutritionSam
                |> unique

        previousPeriodAcuteMalnutritionTotal =
            previousPeriodAcuteMalnutritionMamSam
                ++ previousPeriodMetric.acuteMalnutritionNormal
                |> Set.fromList

        acuteMalnutritionMamTestedInPreviousPeriod =
            Set.intersect (Set.fromList currentPeriodMetric.acuteMalnutritionMam) previousPeriodAcuteMalnutritionTotal

        acuteMalnutritionMamNotIdentifiedInPreviousPeriod =
            Set.diff (Set.fromList currentPeriodMetric.acuteMalnutritionMam) (Set.fromList previousPeriodAcuteMalnutritionMamSam)

        acuteMalnutritionSamTestedInPreviousPeriod =
            Set.intersect (Set.fromList currentPeriodMetric.acuteMalnutritionSam) previousPeriodAcuteMalnutritionTotal

        acuteMalnutritionSamNotIdentifiedInPreviousPeriod =
            Set.diff (Set.fromList currentPeriodMetric.acuteMalnutritionSam) (Set.fromList previousPeriodMetric.acuteMalnutritionSam)
```

And in the returned record, add two more fields (in alphabetical position with respect to the existing fields — i.e. `acuteMalnutrition*` comes before `stunting*`/`wasting*`/`underweight*` if record fields are alphabetical, OR just below the existing fields if they're not. **Read the existing return record first to match its actual ordering.** The record literal looks like:

```elm
    { stuntingModerate = calculatePercentage ...
    , stuntingSevere = calculatePercentage ...
    , wastingModerate = ...
    , wastingSevere = ...
    , underweightModerate = ...
    , underweightSevere = ...
    , acuteMalnutritionMam = calculatePercentage (Set.intersect acuteMalnutritionMamTestedInPreviousPeriod acuteMalnutritionMamNotIdentifiedInPreviousPeriod) previousPeriodAcuteMalnutritionTotal
    , acuteMalnutritionSam = calculatePercentage (Set.intersect acuteMalnutritionSamTestedInPreviousPeriod acuteMalnutritionSamNotIdentifiedInPreviousPeriod) previousPeriodAcuteMalnutritionTotal
    }
```

(`calculatePercentage` for incidence operates on `Set` here, not `List`, per the existing helper inside this function. Confirm by reading the local helper definition before using.)

### Step 3.6 — Integrate family nutrition encounters into the encounters-by-month builder

- [ ] Locate the function in `Pages/Reports/Utils.elm` that folds patient encounters into a `Dict ( Int, Int ) NutritionMetrics` (likely named `generareNutritionReportDataFromRawData` or a helper it calls). Read the section that walks `wellChildData`, `individualNutritionData`, and the group nutrition data sources — it should look something like:

```elm
patientData.wellChildData
    |> Maybe.withDefault []
    |> List.concat
    |> List.map (nutritionEncounterDataToNutritionMetrics patientData.id)
    -- ... fold into dict by month
```

- [ ] Add a parallel branch for family nutrition that uses `familyNutritionEncounterToMetrics` instead. Read the existing builder structure carefully and mirror it. The family nutrition data lives in `patientData.familyNutritionData : Maybe (List (List FamilyNutritionEncounterData))`. Each encounter contributes to the month bucket determined by `encounter.startDate`.

If the existing builder is too tangled to extend mechanically, surface to the user before making the change.

### Step 3.7 — Add Translate variants

- [ ] Open `server/elm/src/Translate.elm`. Locate the `type TranslationId` definition. Insert two new variants in alphabetical position:

```elm
    | AcuteMalnutritionMam
    | AcuteMalnutritionSam
```

(They go before `ACHI` if `ACHI` is currently first, or wherever the alphabetical sort places them. **Read the existing variant list to find the correct insertion point.** Capital letters sort before lowercase per Elm's standard ordering, but the spec convention here is "alphabetical by variant name" — read existing entries to confirm.)

### Step 3.8 — Add Translate set entries

- [ ] In the same file, locate the `translationSet` function's case branches. Insert two new branches in alphabetical position:

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

Per `CLAUDE.md`, leave the non-English fields as `Nothing`.

### Step 3.9 — Extend the raw-data view rows

- [ ] In `server/elm/src/Pages/Reports/Utils.elm`, locate `generareNutritionReportDataFromRawData`. Inside, find the section that calls `generateRow` for each indicator (around line 1374-onwards). Read the section and add two more `generateRow` calls in the same shape, one for `.acuteMalnutritionMam` and one for `.acuteMalnutritionSam`, using the new translations:

```elm
            |> generateRow Translate.AcuteMalnutritionMam .acuteMalnutritionMam

            |> generateRow Translate.AcuteMalnutritionSam .acuteMalnutritionSam
```

(Match the indentation and pipe shape of the existing `generateRow` calls. Read the function first.)

### Step 3.10 — Format

- [ ] Run from `/var/www/html/sec-ihangane`:

```bash
elm-format server/elm/src/Pages/Reports/Model.elm server/elm/src/Pages/Reports/Utils.elm server/elm/src/Translate.elm
```

### Step 3.11 — Verify compilation

- [ ] **Ask the user** to compile and confirm. The most likely failure is an alphabetical-ordering complaint or a missing `Set.fromList` import — surface any errors to the user.

### Step 3.12 — Commit

- [ ] Run:

```bash
git add server/elm/src/Pages/Reports/Model.elm server/elm/src/Pages/Reports/Utils.elm server/elm/src/Translate.elm
git commit -m "$(cat <<'EOF'
Issue #1718: complete small-scope path for MAM/SAM rows

Extends NutritionMetricsResults with acuteMalnutritionMam/Sam,
prevalence and incidence calculations to compute the two new rows,
the encounters-by-month builder to fold family nutrition encounters
in (MAM/SAM only), countTotalNutritionEncounters to include family
nutrition, and the raw-data view to render the two new rows.
Adds AcuteMalnutritionMam/Sam translation strings.

Sector / cell / village reports now show 8 rows per table, populated
once PHP starts emitting MUAC/edema in the next commit.

Refs #1718

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

---

## Task 4 — PHP wire-format writers

Adds MUAC and nutrition-signs collection in `hedley_reports_calculate_aggregated_data_for_person`, extends the wire-format encoders, and adds the new measurement bundles to the trigger list. No report-generation logic changes yet — this commit only changes what's stored on disk.

**Files:**
- Modify: `server/hedley/modules/custom/hedley_reports/hedley_reports.module`

### Step 4.1 — Extend `hedley_reports_get_triggering_measurement_types`

- [ ] Open `server/hedley/modules/custom/hedley_reports/hedley_reports.module`. Locate `hedley_reports_get_triggering_measurement_types` (around line 462). Replace the body with:

```php
function hedley_reports_get_triggering_measurement_types() {
  return array_unique(array_merge(
    HEDLEY_ACTIVITY_HEIGHT_BUNDLES,
    HEDLEY_ACTIVITY_WEIGHT_BUNDLES,
    HEDLEY_ACTIVITY_MUAC_BUNDLES,
    HEDLEY_ACTIVITY_NUTRITION_BUNDLES,
    HEDLEY_ACTIVITY_REPORTS_GROUP_MEASUREMENT_BUNDLES,
  ));
}
```

This adds five measurement content types to the incremental-recalc trigger: `nutrition_muac`, `well_child_muac`, `muac` (group), `nutrition` (group), `nutrition_nutrition`, `well_child_nutrition`.

### Step 4.2 — Add the MUAC and nutrition-signs cases to `hedley_reports_calculate_aggregated_data_for_person`'s measurement switch

- [ ] Locate `hedley_reports_calculate_aggregated_data_for_person` (around line 481). Find the line `$sessions_ids = $zscores_by_encounter = [];` (around line 494) and replace it with:

```php
  $sessions_ids = $zscores_by_encounter = $muac_edema_by_encounter = [];
```

- [ ] Find the measurement-type `switch` block (starts around line 499). After the existing `case HEDLEY_ACTIVITY_WEIGHT_CONTENT_TYPE:` block and before the `default:`, insert six new cases:

```php
      case HEDLEY_ACTIVITY_NUTRITION_MUAC_CONTENT_TYPE:
        $encounter_id = $measurement->field_nutrition_encounter[LANGUAGE_NONE][0]['target_id'];
        $muac_value = $measurement->field_muac[LANGUAGE_NONE][0]['value'];
        if (!is_null($muac_value) && $muac_value !== '') {
          $muac_edema_by_encounter[$encounter_id]['m'] = $muac_value;
        }
        break;

      case HEDLEY_ACTIVITY_WELL_CHILD_MUAC_CONTENT_TYPE:
        $encounter_id = $measurement->field_well_child_encounter[LANGUAGE_NONE][0]['target_id'];
        $muac_value = $measurement->field_muac[LANGUAGE_NONE][0]['value'];
        if (!is_null($muac_value) && $muac_value !== '') {
          $muac_edema_by_encounter[$encounter_id]['m'] = $muac_value;
        }
        break;

      case HEDLEY_ACTIVITY_MUAC_CONTENT_TYPE:
        $encounter_id = $measurement->field_session[LANGUAGE_NONE][0]['target_id'];
        $sessions_ids[] = $encounter_id;
        $muac_value = $measurement->field_muac[LANGUAGE_NONE][0]['value'];
        if (!is_null($muac_value) && $muac_value !== '') {
          $muac_edema_by_encounter[$encounter_id]['m'] = $muac_value;
        }
        break;

      case HEDLEY_ACTIVITY_NUTRITION_NUTRITION_CONTENT_TYPE:
        $encounter_id = $measurement->field_nutrition_encounter[LANGUAGE_NONE][0]['target_id'];
        $signs = !empty($measurement->field_nutrition_signs[LANGUAGE_NONE])
          ? array_column($measurement->field_nutrition_signs[LANGUAGE_NONE], 'value')
          : [];
        $muac_edema_by_encounter[$encounter_id]['e'] = in_array('edema', $signs);
        break;

      case HEDLEY_ACTIVITY_WELL_CHILD_NUTRITION_CONTENT_TYPE:
        $encounter_id = $measurement->field_well_child_encounter[LANGUAGE_NONE][0]['target_id'];
        $signs = !empty($measurement->field_nutrition_signs[LANGUAGE_NONE])
          ? array_column($measurement->field_nutrition_signs[LANGUAGE_NONE], 'value')
          : [];
        $muac_edema_by_encounter[$encounter_id]['e'] = in_array('edema', $signs);
        break;

      case HEDLEY_ACTIVITY_NUTRITION_CONTENT_TYPE:
        $encounter_id = $measurement->field_session[LANGUAGE_NONE][0]['target_id'];
        $sessions_ids[] = $encounter_id;
        $signs = !empty($measurement->field_nutrition_signs[LANGUAGE_NONE])
          ? array_column($measurement->field_nutrition_signs[LANGUAGE_NONE], 'value')
          : [];
        $muac_edema_by_encounter[$encounter_id]['e'] = in_array('edema', $signs);
        break;
```

Note three encounter-parent fields are used:
- `field_nutrition_encounter` for individual nutrition (`nutrition_muac`, `nutrition_nutrition`)
- `field_well_child_encounter` for well-child (`well_child_muac`, `well_child_nutrition`)
- `field_session` for group session (`muac`, `nutrition`)

The group-session cases also push to `$sessions_ids` (matching the existing pattern for group height/weight, so the session is loaded later for group_type lookup).

### Step 4.3 — Pass MUAC/edema to the wire-format encoder for nutrition / well-child encounters

- [ ] Locate the per-encounter loop block in `hedley_reports_calculate_aggregated_data_for_person` that handles nutrition + SPV encounters (around line 646). It currently looks like:

```php
        case HEDLEY_STATS_NUTRITION_ENCOUNTER_TYPE:
        case HEDLEY_STATS_SPV_ENCOUNTER_TYPE:
          $nutrition = hedley_reports_nutrition_metrics_to_string($zscores_by_encounter[$encounter->nid]);
          $encounter_data = "$encounter_date $nutrition";
          break;
```

Replace with:

```php
        case HEDLEY_STATS_NUTRITION_ENCOUNTER_TYPE:
        case HEDLEY_STATS_SPV_ENCOUNTER_TYPE:
          $zscores = isset($zscores_by_encounter[$encounter->nid]) ? $zscores_by_encounter[$encounter->nid] : [];
          $muac_edema = isset($muac_edema_by_encounter[$encounter->nid]) ? $muac_edema_by_encounter[$encounter->nid] : [];
          $nutrition = hedley_reports_nutrition_metrics_to_string($zscores + $muac_edema);
          $encounter_data = "$encounter_date $nutrition";
          break;
```

The `+` array union is order-preserving and lets us pass both the z-scores (`s`, `w`, `u`) and the MUAC/edema (`m`, `e`) in a single map to the encoder.

### Step 4.4 — Pass MUAC/edema for group session encounter strings

- [ ] Locate the group session encounter loop (around line 743–760, after the per-encounter type loop). Find the line:

```php
    $nutrition = hedley_reports_nutrition_metrics_to_string($zscores_by_encounter[$session->nid]);
```

Replace with:

```php
    $zscores = isset($zscores_by_encounter[$session->nid]) ? $zscores_by_encounter[$session->nid] : [];
    $muac_edema = isset($muac_edema_by_encounter[$session->nid]) ? $muac_edema_by_encounter[$session->nid] : [];
    $nutrition = hedley_reports_nutrition_metrics_to_string($zscores + $muac_edema);
```

### Step 4.5 — Extend `hedley_reports_nutrition_metrics_to_string`

- [ ] Locate `hedley_reports_nutrition_metrics_to_string` (around line 808). Replace the function body with:

```php
function hedley_reports_nutrition_metrics_to_string($values) {
  if (empty($values)) {
    return ',,,,';
  }

  $stunting = isset($values['s']) && $values['s'] !== '' ? $values['s'] : '';
  $wasting = isset($values['w']) && $values['w'] !== '' ? $values['w'] : '';
  $underweight = isset($values['u']) && $values['u'] !== '' ? $values['u'] : '';
  $muac = isset($values['m']) && $values['m'] !== '' ? $values['m'] : '';
  $edema = isset($values['e']) ? ($values['e'] ? '1' : '0') : '';

  return "$stunting,$wasting,$underweight,$muac,$edema";
}
```

The empty-default case returns `,,,,` (four commas → five empty fields) instead of the old `,,` (two commas → three empty fields). The 5-field shape matches the new wire format.

**Important:** the existing function uses `!empty()` which treats `0` as empty. We change to `isset() && !== ''` for `m` (so a MUAC of `0` — unlikely but valid input — survives) and explicit conversion for `e` (the boolean from the switch block above). If the executor finds existing call sites that legitimately pass `'s' => 0`, this changes behavior — investigate before altering.

Update the docblock above the function to reflect the new keys (`'m'` for MUAC, `'e'` for edema flag).

### Step 4.6 — Add the family-nutrition encoder helper

- [ ] Immediately below `hedley_reports_nutrition_metrics_to_string`, add:

```php
/**
 * Converts family-nutrition encounter metrics to a wire-format string fragment.
 *
 * Family nutrition has only MUAC (no z-scores, no edema). When MUAC is
 * absent, returns the empty string so the caller emits a date-only encounter.
 *
 * @param array|null $values
 *   Optional 'm' key with the MUAC value in cm.
 *
 * @return string
 *   The MUAC value as a string, or an empty string if absent.
 */
function hedley_reports_family_nutrition_metrics_to_string($values) {
  if (empty($values) || !isset($values['m']) || $values['m'] === '') {
    return '';
  }

  return (string) $values['m'];
}
```

### Step 4.7 — Query family-nutrition MUAC measurements and pass them to the encoder

- [ ] Locate the family-nutrition handling block in `hedley_reports_calculate_aggregated_data_for_person` (around line 694). It currently queries `family_participants` and then `family_encounters`, then writes a date-only string for each. Read the entire block first.

- [ ] Before the `if (!empty($family_participants_result['node']))` block, query family-nutrition MUAC measurements and build a per-encounter MUAC map. Insert (placement: directly before the existing `family_participants` query):

```php
  // Build per-encounter MUAC map for family nutrition.
  $family_muac_by_encounter = [];
  $family_muac_query = hedley_general_create_entity_field_query_excluding_deleted();
  $family_muac_result = $family_muac_query
    ->entityCondition('entity_type', 'node')
    ->entityCondition('bundle', 'family_nutrition_muac_child')
    ->fieldCondition('field_person', 'target_id', $person->nid)
    ->propertyCondition('status', NODE_PUBLISHED)
    ->execute();

  if (!empty($family_muac_result['node'])) {
    $family_muac_ids = array_keys($family_muac_result['node']);
    $family_muac_measurements = node_load_multiple($family_muac_ids);
    foreach ($family_muac_measurements as $family_muac) {
      $encounter_id = $family_muac->field_family_nutrition_encounter[LANGUAGE_NONE][0]['target_id'];
      $muac_value = $family_muac->field_muac[LANGUAGE_NONE][0]['value'];
      if (!is_null($muac_value) && $muac_value !== '') {
        $family_muac_by_encounter[$encounter_id] = $muac_value;
      }
    }
  }
```

- [ ] Now find the existing line:

```php
        $encounters_data['family-nutrition'][$participant_id][] = date("Y-m-d", strtotime($family_encounter->field_scheduled_date[LANGUAGE_NONE][0]['value']));
```

Replace it with:

```php
        $encounter_date = date("Y-m-d", strtotime($family_encounter->field_scheduled_date[LANGUAGE_NONE][0]['value']));
        $muac_str = hedley_reports_family_nutrition_metrics_to_string(
          isset($family_muac_by_encounter[$family_encounter->nid]) ? ['m' => $family_muac_by_encounter[$family_encounter->nid]] : NULL
        );
        $encounters_data['family-nutrition'][$participant_id][] = $muac_str === '' ? $encounter_date : "$encounter_date $muac_str";
```

When MUAC is absent, we emit the date-only legacy form (`"YYYY-MM-DD"`) — exactly what the spec specifies for the "family nutrition (no MUAC)" case.

### Step 4.8 — Lint check

- [ ] Run from `/var/www/html/sec-ihangane`:

```bash
REVIEW_STANDARD="Drupal" ci-scripts/test_coder.sh
```

Expected: zero errors. Common issues to fix in place if reported: trailing whitespace, missing docblocks, line length.

- [ ] Also run:

```bash
REVIEW_STANDARD="DrupalPractice" ci-scripts/test_coder.sh
```

Expected: zero errors.

### Step 4.9 — Commit

- [ ] Run:

```bash
git add server/hedley/modules/custom/hedley_reports/hedley_reports.module
git commit -m "$(cat <<'EOF'
Issue #1718: emit MUAC and edema in nutrition wire format

Extends hedley_reports_calculate_aggregated_data_for_person to collect
MUAC measurements (nutrition_muac, well_child_muac, muac) and nutrition
signs measurements (nutrition_nutrition, well_child_nutrition, nutrition)
into a per-encounter map, queries family_nutrition_muac_child for
family nutrition encounters, and emits the new wire formats:
"YYYY-MM-DD s,u,w,muac,e" for nutrition / well-child / group encounters
and "YYYY-MM-DD muac" for family nutrition (with date-only fallback
when MUAC is absent).

Adds the new measurement bundles to the incremental recalc trigger
list.

No report-generation logic changes in this commit; the new fields are
written but not yet read by the report aggregator.

Refs #1718

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

---

## Task 5 — PHP nutrition report computation

Adds the categorize helper, extends the metric helpers, and updates `hedley_reports_generate_nutrition_report_data` to parse the new 5-field wire format and process family nutrition encounters. After this commit, the precomputed `report_data` JSON will include `acute_malnutrition_mam` and `acute_malnutrition_sam` keys.

**Files:**
- Modify: `server/hedley/modules/custom/hedley_reports/hedley_reports.module`

### Step 5.1 — Add `hedley_reports_categorize_acute_malnutrition`

- [ ] Locate `hedley_reports_categorize_z_score` (around line 2162). Immediately below it, insert:

```php
/**
 * Categorizes a MUAC measurement (with optional edema) into MAM/SAM/normal.
 *
 * @param float|null $muac_cm
 *   The MUAC value in centimeters, or NULL if no MUAC was recorded.
 * @param bool $has_edema
 *   TRUE if edema is present.
 * @param int $person_id
 *   The ID of the person whose measurement is being categorized.
 *
 * @return array
 *   A three-element array of [normal, mam, sam] sub-arrays. When MUAC is
 *   missing, all three are empty (the encounter does not enter the AM
 *   denominator at all).
 */
function hedley_reports_categorize_acute_malnutrition($muac_cm, $has_edema, $person_id) {
  if ($muac_cm === NULL || $muac_cm === '') {
    return [[], [], []];
  }

  $muac_mm = (float) $muac_cm * 10;

  if ($muac_mm < 115 || $has_edema) {
    return [[], [], [$person_id]];
  }

  if ($muac_mm < 125) {
    return [[], [$person_id], []];
  }

  return [[$person_id], [], []];
}
```

### Step 5.2 — Extend `hedley_reports_empty_nutrition_metrics`

- [ ] Locate `hedley_reports_empty_nutrition_metrics` (around line 2188). Replace its body with:

```php
function hedley_reports_empty_nutrition_metrics() {
  return [
    'stunting_normal' => [],
    'stunting_moderate' => [],
    'stunting_severe' => [],
    'wasting_normal' => [],
    'wasting_moderate' => [],
    'wasting_severe' => [],
    'underweight_normal' => [],
    'underweight_moderate' => [],
    'underweight_severe' => [],
    'acute_malnutrition_normal' => [],
    'acute_malnutrition_mam' => [],
    'acute_malnutrition_sam' => [],
  ];
}
```

### Step 5.3 — Extend `hedley_reports_sum_nutrition_metrics`

- [ ] Locate `hedley_reports_sum_nutrition_metrics` (around line 2129). Add three more `array_merge` lines to the body:

```php
function hedley_reports_sum_nutrition_metrics(array $metrics_list) {
  $result = hedley_reports_empty_nutrition_metrics();

  foreach ($metrics_list as $metrics) {
    $result['stunting_normal'] = array_merge($result['stunting_normal'], $metrics['stunting_normal']);
    $result['stunting_moderate'] = array_merge($result['stunting_moderate'], $metrics['stunting_moderate']);
    $result['stunting_severe'] = array_merge($result['stunting_severe'], $metrics['stunting_severe']);
    $result['wasting_normal'] = array_merge($result['wasting_normal'], $metrics['wasting_normal']);
    $result['wasting_moderate'] = array_merge($result['wasting_moderate'], $metrics['wasting_moderate']);
    $result['wasting_severe'] = array_merge($result['wasting_severe'], $metrics['wasting_severe']);
    $result['underweight_normal'] = array_merge($result['underweight_normal'], $metrics['underweight_normal']);
    $result['underweight_moderate'] = array_merge($result['underweight_moderate'], $metrics['underweight_moderate']);
    $result['underweight_severe'] = array_merge($result['underweight_severe'], $metrics['underweight_severe']);
    $result['acute_malnutrition_normal'] = array_merge($result['acute_malnutrition_normal'], $metrics['acute_malnutrition_normal']);
    $result['acute_malnutrition_mam'] = array_merge($result['acute_malnutrition_mam'], $metrics['acute_malnutrition_mam']);
    $result['acute_malnutrition_sam'] = array_merge($result['acute_malnutrition_sam'], $metrics['acute_malnutrition_sam']);
  }

  return $result;
}
```

### Step 5.4 — Extend `hedley_reports_nutrition_encounter_data_to_nutrition_metrics`

- [ ] Locate `hedley_reports_nutrition_encounter_data_to_nutrition_metrics` (around line 2100). Replace it with:

```php
function hedley_reports_nutrition_encounter_data_to_nutrition_metrics($person_id, array $data) {
  [$stunting_normal, $stunting_moderate, $stunting_severe] = hedley_reports_categorize_z_score($data['stunting'], $person_id);
  [$wasting_normal, $wasting_moderate, $wasting_severe] = hedley_reports_categorize_z_score($data['wasting'], $person_id);
  [$underweight_normal, $underweight_moderate, $underweight_severe] = hedley_reports_categorize_z_score($data['underweight'], $person_id);

  $muac_cm = isset($data['muac']) ? $data['muac'] : NULL;
  $has_edema = !empty($data['edema']);
  [$acute_malnutrition_normal, $acute_malnutrition_mam, $acute_malnutrition_sam] = hedley_reports_categorize_acute_malnutrition($muac_cm, $has_edema, $person_id);

  return [
    'stunting_normal' => $stunting_normal,
    'stunting_moderate' => $stunting_moderate,
    'stunting_severe' => $stunting_severe,
    'wasting_normal' => $wasting_normal,
    'wasting_moderate' => $wasting_moderate,
    'wasting_severe' => $wasting_severe,
    'underweight_normal' => $underweight_normal,
    'underweight_moderate' => $underweight_moderate,
    'underweight_severe' => $underweight_severe,
    'acute_malnutrition_normal' => $acute_malnutrition_normal,
    'acute_malnutrition_mam' => $acute_malnutrition_mam,
    'acute_malnutrition_sam' => $acute_malnutrition_sam,
  ];
}
```

### Step 5.5 — Extend `hedley_reports_generate_prevalence_nutrition_metrics_results`

- [ ] Locate `hedley_reports_generate_prevalence_nutrition_metrics_results` (around line 1849). Replace it with:

```php
function hedley_reports_generate_prevalence_nutrition_metrics_results(array $metrics) {
  $stunting_total = array_unique(array_merge($metrics['stunting_moderate'], $metrics['stunting_severe'], $metrics['stunting_normal']));
  $wasting_total = array_unique(array_merge($metrics['wasting_moderate'], $metrics['wasting_severe'], $metrics['wasting_normal']));
  $underweight_total = array_unique(array_merge($metrics['underweight_moderate'], $metrics['underweight_severe'], $metrics['underweight_normal']));
  $acute_malnutrition_total = array_unique(array_merge($metrics['acute_malnutrition_mam'], $metrics['acute_malnutrition_sam'], $metrics['acute_malnutrition_normal']));

  return [
    'stunting_moderate' => hedley_reports_calculate_percentage($metrics['stunting_moderate'], $stunting_total),
    'stunting_severe' => hedley_reports_calculate_percentage($metrics['stunting_severe'], $stunting_total),
    'wasting_moderate' => hedley_reports_calculate_percentage($metrics['wasting_moderate'], $wasting_total),
    'wasting_severe' => hedley_reports_calculate_percentage($metrics['wasting_severe'], $wasting_total),
    'underweight_moderate' => hedley_reports_calculate_percentage($metrics['underweight_moderate'], $underweight_total),
    'underweight_severe' => hedley_reports_calculate_percentage($metrics['underweight_severe'], $underweight_total),
    'acute_malnutrition_mam' => hedley_reports_calculate_percentage($metrics['acute_malnutrition_mam'], $acute_malnutrition_total),
    'acute_malnutrition_sam' => hedley_reports_calculate_percentage($metrics['acute_malnutrition_sam'], $acute_malnutrition_total),
  ];
}
```

### Step 5.6 — Extend `hedley_reports_generate_incidence_nutrition_metrics_results`

- [ ] Locate `hedley_reports_generate_incidence_nutrition_metrics_results` (around line 1875). Read it fully — it has stunting, wasting, and underweight blocks plus a return record. Add a fourth block parallel to the others. Replace the function with:

```php
function hedley_reports_generate_incidence_nutrition_metrics_results(array $current_period_metric, array $previous_period_metric) {
  // STUNTING.
  $previous_period_stunting_moderate_severe = array_unique(array_merge($previous_period_metric['stunting_moderate'], $previous_period_metric['stunting_severe']));
  $previous_period_stunting_total = array_unique(array_merge($previous_period_stunting_moderate_severe, $previous_period_metric['stunting_normal']));

  $stunting_moderate_tested_in_previous_period = array_intersect(array_unique($current_period_metric['stunting_moderate']), $previous_period_stunting_total);
  $stunting_moderate_not_identified_in_previous_period = array_diff(array_unique($current_period_metric['stunting_moderate']), $previous_period_stunting_moderate_severe);

  $stunting_severe_tested_in_previous_period = array_intersect(array_unique($current_period_metric['stunting_severe']), $previous_period_stunting_total);
  $stunting_severe_not_identified_in_previous_period = array_diff(array_unique($current_period_metric['stunting_severe']), array_unique($previous_period_metric['stunting_severe']));

  // WASTING.
  $previous_period_wasting_moderate_severe = array_unique(array_merge($previous_period_metric['wasting_moderate'], $previous_period_metric['wasting_severe']));
  $previous_period_wasting_total = array_unique(array_merge($previous_period_wasting_moderate_severe, $previous_period_metric['wasting_normal']));

  $wasting_moderate_tested_in_previous_period = array_intersect(array_unique($current_period_metric['wasting_moderate']), $previous_period_wasting_total);
  $wasting_moderate_not_identified_in_previous_period = array_diff(array_unique($current_period_metric['wasting_moderate']), $previous_period_wasting_moderate_severe);

  $wasting_severe_tested_in_previous_period = array_intersect(array_unique($current_period_metric['wasting_severe']), $previous_period_wasting_total);
  $wasting_severe_not_identified_in_previous_period = array_diff(array_unique($current_period_metric['wasting_severe']), array_unique($previous_period_metric['wasting_severe']));

  // UNDERWEIGHT.
  $previous_period_underweight_moderate_severe = array_unique(array_merge($previous_period_metric['underweight_moderate'], $previous_period_metric['underweight_severe']));
  $previous_period_underweight_total = array_unique(array_merge($previous_period_underweight_moderate_severe, $previous_period_metric['underweight_normal']));

  $underweight_moderate_tested_in_previous_period = array_intersect(array_unique($current_period_metric['underweight_moderate']), $previous_period_underweight_total);
  $underweight_moderate_not_identified_in_previous_period = array_diff(array_unique($current_period_metric['underweight_moderate']), $previous_period_underweight_moderate_severe);

  $underweight_severe_tested_in_previous_period = array_intersect(array_unique($current_period_metric['underweight_severe']), $previous_period_underweight_total);
  $underweight_severe_not_identified_in_previous_period = array_diff(array_unique($current_period_metric['underweight_severe']), array_unique($previous_period_metric['underweight_severe']));

  // ACUTE MALNUTRITION.
  $previous_period_acute_malnutrition_mam_sam = array_unique(array_merge($previous_period_metric['acute_malnutrition_mam'], $previous_period_metric['acute_malnutrition_sam']));
  $previous_period_acute_malnutrition_total = array_unique(array_merge($previous_period_acute_malnutrition_mam_sam, $previous_period_metric['acute_malnutrition_normal']));

  $acute_malnutrition_mam_tested_in_previous_period = array_intersect(array_unique($current_period_metric['acute_malnutrition_mam']), $previous_period_acute_malnutrition_total);
  $acute_malnutrition_mam_not_identified_in_previous_period = array_diff(array_unique($current_period_metric['acute_malnutrition_mam']), $previous_period_acute_malnutrition_mam_sam);

  $acute_malnutrition_sam_tested_in_previous_period = array_intersect(array_unique($current_period_metric['acute_malnutrition_sam']), $previous_period_acute_malnutrition_total);
  $acute_malnutrition_sam_not_identified_in_previous_period = array_diff(array_unique($current_period_metric['acute_malnutrition_sam']), array_unique($previous_period_metric['acute_malnutrition_sam']));

  return [
    'stunting_moderate' => hedley_reports_calculate_percentage(array_intersect($stunting_moderate_tested_in_previous_period, $stunting_moderate_not_identified_in_previous_period), $previous_period_stunting_total),
    'stunting_severe' => hedley_reports_calculate_percentage(array_intersect($stunting_severe_tested_in_previous_period, $stunting_severe_not_identified_in_previous_period), $previous_period_stunting_total),
    'wasting_moderate' => hedley_reports_calculate_percentage(array_intersect($wasting_moderate_tested_in_previous_period, $wasting_moderate_not_identified_in_previous_period), $previous_period_wasting_total),
    'wasting_severe' => hedley_reports_calculate_percentage(array_intersect($wasting_severe_tested_in_previous_period, $wasting_severe_not_identified_in_previous_period), $previous_period_wasting_total),
    'underweight_moderate' => hedley_reports_calculate_percentage(array_intersect($underweight_moderate_tested_in_previous_period, $underweight_moderate_not_identified_in_previous_period), $previous_period_underweight_total),
    'underweight_severe' => hedley_reports_calculate_percentage(array_intersect($underweight_severe_tested_in_previous_period, $underweight_severe_not_identified_in_previous_period), $previous_period_underweight_total),
    'acute_malnutrition_mam' => hedley_reports_calculate_percentage(array_intersect($acute_malnutrition_mam_tested_in_previous_period, $acute_malnutrition_mam_not_identified_in_previous_period), $previous_period_acute_malnutrition_total),
    'acute_malnutrition_sam' => hedley_reports_calculate_percentage(array_intersect($acute_malnutrition_sam_tested_in_previous_period, $acute_malnutrition_sam_not_identified_in_previous_period), $previous_period_acute_malnutrition_total),
  ];
}
```

### Step 5.7 — Extend `hedley_reports_nutrition_metrics_to_table_data`

- [ ] Locate `hedley_reports_nutrition_metrics_to_table_data` (around line 1678). Replace it with:

```php
function hedley_reports_nutrition_metrics_to_table_data($type, array $metrics) {
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

  foreach ($metrics as $key => $values) {
    $result['period'][] = (string) $key;
    $result['stunting_moderate'][] = $values['stunting_moderate'];
    $result['stunting_severe'][] = $values['stunting_severe'];
    $result['wasting_moderate'][] = $values['wasting_moderate'];
    $result['wasting_severe'][] = $values['wasting_severe'];
    $result['underweight_moderate'][] = $values['underweight_moderate'];
    $result['underweight_severe'][] = $values['underweight_severe'];
    $result['acute_malnutrition_mam'][] = $values['acute_malnutrition_mam'];
    $result['acute_malnutrition_sam'][] = $values['acute_malnutrition_sam'];
  }

  return $result;
}
```

### Step 5.8 — Extend `hedley_reports_generate_nutrition_report_data` for the 5-field parse + family encounters

- [ ] Locate `hedley_reports_generate_nutrition_report_data` (around line 1525). Read the entire function. Two changes are needed:

**Change A — Collect family-nutrition encounters into the patient bundle.**

Inside the `foreach ($data as $patient_data)` loop, after the existing block that merges `$patient_data->individual->{"well-child"}` into `$data_for_patient['encounters']`, insert:

```php
    $family_encounters = [];
    if (isset($patient_data->individual)) {
      if (isset($patient_data->individual->{"family-nutrition"})) {
        foreach ($patient_data->individual->{"family-nutrition"} as $items) {
          $family_encounters = array_merge($family_encounters, $items);
        }
      }
    }
    $data_for_patient['family_encounters'] = $family_encounters;
```

**Change B — Parse the new 5-field nutrition format and add a family-nutrition processing pass.**

Locate the per-encounter parse block (around line 1602):

```php
    foreach ($nutrition_data_item['encounters'] as $index => $encounter_data) {
      $parts = explode(' ', $encounter_data);
      $start_date_as_string = $parts[0];
      ...
      $encounter_zscores = explode(',', $parts[1]);
      $nutrition_data = [
        'stunting' => ($encounter_zscores[0] === '') ? NULL : (float) $encounter_zscores[0],
        'underweight' => ($encounter_zscores[1] === '') ? NULL : (float) $encounter_zscores[1],
        'wasting' => ($encounter_zscores[2] === '') ? NULL : (float) $encounter_zscores[2],
      ];
      ...
    }
```

Replace the `$nutrition_data` array assignment with code that handles both 3-field and 5-field formats:

```php
      $encounter_fields = explode(',', $parts[1]);
      $nutrition_data = [
        'stunting' => (!isset($encounter_fields[0]) || $encounter_fields[0] === '') ? NULL : (float) $encounter_fields[0],
        'underweight' => (!isset($encounter_fields[1]) || $encounter_fields[1] === '') ? NULL : (float) $encounter_fields[1],
        'wasting' => (!isset($encounter_fields[2]) || $encounter_fields[2] === '') ? NULL : (float) $encounter_fields[2],
        'muac' => (!isset($encounter_fields[3]) || $encounter_fields[3] === '') ? NULL : (float) $encounter_fields[3],
        'edema' => (isset($encounter_fields[4]) && $encounter_fields[4] === '1'),
      ];
```

(Note: this rewrites `$encounter_zscores` to `$encounter_fields` to better reflect that it's not just z-scores anymore.)

After the existing per-encounter loop (which folds nutrition encounters into `$encounters_by_month`), insert a second loop that processes family encounters:

```php
    foreach ($nutrition_data_item['family_encounters'] as $family_encounter_data) {
      $parts = explode(' ', $family_encounter_data);
      $start_date_as_string = $parts[0];
      $start_date = DateTime::createFromFormat('Y-m-d', $start_date_as_string);
      if ($start_date === FALSE) {
        continue;
      }
      $start_date_year = $start_date->format('Y');
      if ($start_date_year < $starting_year) {
        continue;
      }

      $start_date_month = $start_date->format('m');
      $muac_value = isset($parts[1]) && $parts[1] !== '' ? (float) $parts[1] : NULL;
      $family_data = [
        'stunting' => NULL,
        'underweight' => NULL,
        'wasting' => NULL,
        'muac' => $muac_value,
        'edema' => FALSE,
      ];
      $encounter_metrics = hedley_reports_nutrition_encounter_data_to_nutrition_metrics($nutrition_data_item['id'], $family_data);
      $key = "$start_date_year-$start_date_month";
      if (isset($encounters_by_month[$key])) {
        $encounters_by_month[$key] = hedley_reports_sum_nutrition_metrics([$encounters_by_month[$key], $encounter_metrics]);
      }
      else {
        $encounters_by_month[$key] = $encounter_metrics;
      }
    }
```

This loop produces metrics records where the z-score buckets are empty (because `'stunting'/'underweight'/'wasting'` are all `NULL`, and `categorize_z_score` returns empty arrays for `NULL`) and only the AM buckets are populated. By construction it cannot influence the existing rows.

### Step 5.9 — Extend `$encounters_by_month_for_impacted` to include the new keys

- [ ] Inside `hedley_reports_generate_nutrition_report_data`, locate the loop that builds `$encounters_by_month_for_impacted` (around line 1631). Add three more keys:

```php
  foreach ($encounters_by_month as $key => $encounter) {
    $encounters_by_month_for_impacted[$key] = [
      'stunting_normal' => hedley_reports_filter_impacted_ids($encounter['stunting_normal'], $impacted),
      'stunting_moderate' => hedley_reports_filter_impacted_ids($encounter['stunting_moderate'], $impacted),
      'stunting_severe' => hedley_reports_filter_impacted_ids($encounter['stunting_severe'], $impacted),
      'wasting_normal' => hedley_reports_filter_impacted_ids($encounter['wasting_normal'], $impacted),
      'wasting_moderate' => hedley_reports_filter_impacted_ids($encounter['wasting_moderate'], $impacted),
      'wasting_severe' => hedley_reports_filter_impacted_ids($encounter['wasting_severe'], $impacted),
      'underweight_normal' => hedley_reports_filter_impacted_ids($encounter['underweight_normal'], $impacted),
      'underweight_moderate' => hedley_reports_filter_impacted_ids($encounter['underweight_moderate'], $impacted),
      'underweight_severe' => hedley_reports_filter_impacted_ids($encounter['underweight_severe'], $impacted),
      'acute_malnutrition_normal' => hedley_reports_filter_impacted_ids($encounter['acute_malnutrition_normal'], $impacted),
      'acute_malnutrition_mam' => hedley_reports_filter_impacted_ids($encounter['acute_malnutrition_mam'], $impacted),
      'acute_malnutrition_sam' => hedley_reports_filter_impacted_ids($encounter['acute_malnutrition_sam'], $impacted),
    ];
  }
```

Otherwise the prevalence-2 / incidence-2 (two-visits-or-more) tables would crash on the new keys.

### Step 5.10 — Lint check

- [ ] Run from `/var/www/html/sec-ihangane`:

```bash
REVIEW_STANDARD="Drupal" ci-scripts/test_coder.sh
REVIEW_STANDARD="DrupalPractice" ci-scripts/test_coder.sh
```

Expected: zero errors. Fix in place.

### Step 5.11 — Commit

- [ ] Run:

```bash
git add server/hedley/modules/custom/hedley_reports/hedley_reports.module
git commit -m "$(cat <<'EOF'
Issue #1718: compute MAM/SAM rows in PHP nutrition report

Adds hedley_reports_categorize_acute_malnutrition (MUAC + edema →
buckets), extends empty/sum/encounter→metrics helpers and the
prevalence/incidence/table_data generators with the two new rows.
hedley_reports_generate_nutrition_report_data now parses the new
5-field nutrition format and folds family nutrition encounters into
the encounters-by-month dict (MAM/SAM only — z-score buckets stay
empty by construction). The encounters_by_month_for_impacted builder
also forwards the new keys so the two-visits-or-more tables work.

After this commit and a recalc-large-datasets run, the precomputed
report_data nodes contain acute_malnutrition_mam and acute_malnutrition_sam
keys. Elm decoder for these is added in the next commit.

Refs #1718

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

---

## Task 6 — Elm large-scope path completion

Adds the new fields to `BackendGeneratedNutritionReportTableDate`, decodes them as `required`, and renders them in the body of the table-data builder used for global / province / district / health-center reports.

**Files:**
- Modify: `server/elm/src/Backend/Reports/Model.elm` (around lines 231–240)
- Modify: `server/elm/src/Backend/Reports/Decoder.elm` (around lines 551–562)
- Modify: `server/elm/src/Pages/Reports/Utils.elm` (around lines 1153–1196: `backendGeneratedNutritionReportTableDateToMetricsResultsTableData`)

### Step 6.1 — Extend `BackendGeneratedNutritionReportTableDate`

- [ ] In `server/elm/src/Backend/Reports/Model.elm`, locate `BackendGeneratedNutritionReportTableDate` (around line 231). Replace with:

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

### Step 6.2 — Extend `decodeBackendGeneratedNutritionReportTableDate`

- [ ] In `server/elm/src/Backend/Reports/Decoder.elm`, locate `decodeBackendGeneratedNutritionReportTableDate` (around line 551). Replace with:

```elm
decodeBackendGeneratedNutritionReportTableDate : Decoder BackendGeneratedNutritionReportTableDate
decodeBackendGeneratedNutritionReportTableDate =
    succeed BackendGeneratedNutritionReportTableDate
        |> required "type" decodeNutritionReportTableType
        |> required "period" (list string)
        |> required "stunting_moderate" (list string)
        |> required "stunting_severe" (list string)
        |> required "wasting_moderate" (list string)
        |> required "wasting_severe" (list string)
        |> required "underweight_moderate" (list string)
        |> required "underweight_severe" (list string)
        |> required "acute_malnutrition_mam" (list string)
        |> required "acute_malnutrition_sam" (list string)
```

### Step 6.3 — Extend `backendGeneratedNutritionReportTableDateToMetricsResultsTableData`

- [ ] In `server/elm/src/Pages/Reports/Utils.elm`, locate `backendGeneratedNutritionReportTableDateToMetricsResultsTableData` (around line 1153). Read the existing function. The body list (the value passed to the `MetricsResultsTableData` record's `body` field) currently has six entries. Add two more, in the same shape:

```elm
    { heading = translate language <| Translate.NutritionReportTableType backendTableData.tableType
    , captions = ...  -- (unchanged)
    , body =
        [ translate language Translate.StuntingModerate :: backendTableData.stuntingModerate
        , translate language Translate.StuntingSevere :: backendTableData.stuntingSevere
        , translate language Translate.WastingModerate :: backendTableData.wastingModerate
        , translate language Translate.WastingSevere :: backendTableData.wastingSevere
        , translate language Translate.UnderweightModerate :: backendTableData.underweightModerate
        , translate language Translate.UnderweightSevere :: backendTableData.underweightSevere
        , translate language Translate.AcuteMalnutritionMam :: backendTableData.acuteMalnutritionMam
        , translate language Translate.AcuteMalnutritionSam :: backendTableData.acuteMalnutritionSam
        ]
    }
```

(Read the function first to confirm exact field names — `body`, `captions`, etc. The existing six lines should look exactly like the first six lines above; if not, match the existing style.)

### Step 6.4 — Format

- [ ] Run from `/var/www/html/sec-ihangane`:

```bash
elm-format server/elm/src/Backend/Reports/Model.elm server/elm/src/Backend/Reports/Decoder.elm server/elm/src/Pages/Reports/Utils.elm
```

### Step 6.5 — Verify compilation

- [ ] **Ask the user** to compile the admin Elm app and confirm.

### Step 6.6 — Commit

- [ ] Run:

```bash
git add server/elm/src/Backend/Reports/Model.elm server/elm/src/Backend/Reports/Decoder.elm server/elm/src/Pages/Reports/Utils.elm
git commit -m "$(cat <<'EOF'
Issue #1718: render MAM/SAM rows in large-scope reports

Extends BackendGeneratedNutritionReportTableDate with
acuteMalnutritionMam and acuteMalnutritionSam (decoded as required)
and adds the two new rows to the body produced by
backendGeneratedNutritionReportTableDateToMetricsResultsTableData.

After this commit, global / province / district / health-center
reports show 8 rows per table once the report_data nodes have been
regenerated by recalculate-large-datasets.

Refs #1718

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

---

## Task 7 — Final lint pass

### Step 7.1 — PHP lint

- [ ] Run from `/var/www/html/sec-ihangane`:

```bash
REVIEW_STANDARD="Drupal" ci-scripts/test_coder.sh
REVIEW_STANDARD="DrupalPractice" ci-scripts/test_coder.sh
```

Expected: zero errors. If anything pops, fix in place and amend with a follow-up commit (do **not** `git commit --amend`).

### Step 7.2 — Elm format validation

- [ ] Run from `/var/www/html/sec-ihangane`:

```bash
elm-format --validate server/elm/src/
```

Expected: zero errors. If formatting drift was reintroduced, run `elm-format server/elm/src/` and commit the formatting fix.

### Step 7.3 — Shell lint (only if any shell scripts were touched)

- [ ] If you modified any shell scripts (you shouldn't have for this issue), run:

```bash
ci-scripts/test_shell.sh
```

### Step 7.4 — Commit any lint fixes

- [ ] If steps 7.1 or 7.2 produced fixups, commit them:

```bash
git add <files>
git commit -m "$(cat <<'EOF'
Issue #1718: lint fixes

Refs #1718

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

If no fixups were needed, skip this step entirely.

---

## Task 8 — Local recalc + manual smoke test

This task does **not** produce a commit. It validates the work end-to-end against a local DDEV environment.

### Step 8.1 — Recalculate per-person reports data

- [ ] Run:

```bash
ddev drush scr profiles/hedley/modules/custom/hedley_reports/scripts/generate-data-for-all.php
```

Expected: progress output like `Calculated so far: NNN, Memory: MMMmb` repeated until `Done! Reports data calculated for NNN children.`. If it stops with "Stopped before out of memory", note the last NID and resume with `--nid=<last>`.

### Step 8.2 — Recalculate large-scope precomputed tables

- [ ] Run:

```bash
ddev drush scr profiles/hedley/modules/custom/hedley_reports/scripts/recalculate-large-datasets.php
```

Expected: progress output for each scope (global, each province, each district, each health center). Should complete without errors.

### Step 8.3 — Build admin Elm app for production

- [ ] **Ask the user** to run the build pipeline (likely `ddev gulp publish` or similar) so the new admin Elm code is compiled into `elm-main.js`. Wait for confirmation.

### Step 8.4 — Smoke test: small-scope path

- [ ] In a browser, log in as a user with the "Statistical Queries Manager" role.
- [ ] Navigate to `admin/reports/statistical-queries/demographics/<province>/<district>/<sector>/<cell>` (a cell scope — small enough to compute on the fly). Optionally also a village scope.
- [ ] Confirm each of the 8 nutrition tables shows 8 rows: stunting moderate, stunting severe, wasting moderate, wasting severe, underweight moderate, underweight severe, **MAM (Moderate Acute Malnutrition)**, **SAM (Severe Acute Malnutrition)**.
- [ ] Pick a period column with non-zero existing rows and confirm the MAM/SAM cells display either a percentage or `0%` (not `NaN`, not blank, not `null`).

### Step 8.5 — Smoke test: large-scope path

- [ ] Navigate to `admin/reports/statistical-queries/all` (global scope). Repeat the row check.
- [ ] Navigate to `admin/reports/statistical-queries/demographics/<province>` (province scope). Repeat.
- [ ] Navigate to `admin/reports/statistical-queries/health-center/<id>` for one health center. Repeat.

### Step 8.6 — Smoke test: family nutrition contribution

- [ ] If a test patient with a family-nutrition encounter that has MUAC is available locally, verify that the MAM/SAM denominator increases by exactly one when that encounter falls within the report's period. (Check by viewing two scopes — one that includes the patient's village, one that doesn't.)
- [ ] Verify that the same patient does **not** affect the stunting/wasting/underweight rows from the family-nutrition encounter alone.

If no such test patient exists locally, surface this to the user — they may want to create one or accept that this branch of behavior is verified only via the parallel structure.

### Step 8.7 — Smoke test: missing-MUAC encounter

- [ ] Find or create a nutrition encounter with height + weight saved but no MUAC tape. Verify the encounter still contributes to stunting/wasting/underweight rows but does **not** appear in the MAM/SAM denominator (the MAM/SAM percentages should be unchanged regardless of whether this encounter exists in the dataset).

### Step 8.8 — Report findings

- [ ] Surface the smoke-test results (pass / fail per row check) to the user. If anything failed, do **not** mark this task as complete — diagnose the failure first and fix forward.

---

## Self-review checklist (run before declaring the plan done)

### Spec coverage

| Spec section | Implemented in |
|---|---|
| Section 1: data capture (MUAC + edema queries, wire format) | Task 4 (PHP), Task 1 (Elm decoder tolerance) |
| Section 2: categorization helpers (PHP + Elm) | Task 5.1 (PHP), Task 2.4 (Elm) |
| Section 3: prevalence + incidence + result type | Task 5.5–5.6 (PHP), Task 3.1–3.5 (Elm) |
| Section 4: encounter collection (PHP family loop, Elm family integration, decoder updates) | Task 5.8 (PHP), Tasks 1, 2.6, 3.6 (Elm) |
| Section 5: pre-computed table data | Task 5.7 (PHP), Task 6.1–6.2 (Elm) |
| Section 6: view rendering + translations | Task 6.3, 3.7–3.9 (Elm) |
| Section 7: deploy/recalc procedure | Task 8 (manual smoke test using existing scripts) |

All sections covered.

### Type consistency

- PHP keys: `acute_malnutrition_normal`, `acute_malnutrition_mam`, `acute_malnutrition_sam` (snake_case) — used consistently in Tasks 4, 5.
- Elm record fields: `acuteMalnutritionNormal`, `acuteMalnutritionMam`, `acuteMalnutritionSam` (camelCase) — used consistently in Tasks 1, 2, 3, 6.
- Elm TranslationId: `AcuteMalnutritionMam`, `AcuteMalnutritionSam` (PascalCase, not all-caps) — used consistently in Tasks 3, 6.
- Helper names: `hedley_reports_categorize_acute_malnutrition` / `categorizeAcuteMalnutrition` — defined and called consistently.
- Family helper: `familyNutritionEncounterToMetrics` — defined Task 2.6, used Task 3.6.

### Things deliberately not in this plan

- New automated tests (per spec).
- Renaming the misleading `'w'`/`'u'` PHP variable nomenclature (per spec out-of-scope).
- Localized translations for Kinyarwanda/Kirundi/Somali (per spec).
- Touching any non-statistical-queries report (per spec).

### Risks and unknowns left to discover during implementation

- **Existing call sites of `nutritionEncounterDataToNutritionMetrics`** (Elm, Task 2.5) — the function signature stays the same but its inner shape changed from a `>>` composition to direct application on the whole record. Compilation will catch any incompatible call site.
- **Existing usages of `FamilyNutritionEncounterData` as a bare `NominalDate`** (Elm, Task 1.10) — handled explicitly as a verification step.
- **The encounters-by-month builder** (Elm, Task 3.6) — the plan describes the change conceptually but defers the exact code shape until the executor reads the actual function. The risk is this function turns out to be more tangled than expected; the plan instructs the executor to surface that to the user rather than improvise.
- **Memory pressure from `generate-data-for-all.php`** on a large local database (Task 8.1). The script is resumable; if it stops, restart with `--nid=<last>`.
