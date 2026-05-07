# "Unable to take measurement" for Well Child Vitals (CHW) — null-field design

**Date:** 2026-04-28
**Issue:** [#1471](https://github.com/TIP-Global-Health/eheza-app/issues/1471)
**Stacked on:** [PR #1653](https://github.com/TIP-Global-Health/eheza-app/pull/1653) (branch `revert-1641-restore-issue-1448`)

## Background

PR #1653 (unmerged) introduced an "Unable to take measurement" checkbox for
Height and Weight in CHW Nutrition / Well Child encounters. The pattern PR
#1653 established for **single-measurement** forms is:

- New Elm type `SkippedForm = SkippedHeight | SkippedWeight`.
- An `EverySet SkippedForm` field on `NutritionEncounter` and
  `WellChildEncounter` records, persisted as Drupal multi-value field
  `field_skipped_forms` with allowed values `height`, `weight`.
- When a measurement is skipped: **no measurement node saved**. The
  encounter records the skip in its `skipped_forms` set so the activity
  still counts as complete.
- When a measurement is entered: the measurement node is saved, and the
  encounter's skip set has the entry removed if it was previously set.

This works for Height/Weight because each form has exactly one measurement;
"no row saved" is a natural way to represent skipping. The encounter-level
skip set distinguishes "deliberately skipped" from "not yet entered."

## Requirement

A CHW (Burundi or Rwanda in practice — gated only by `isChw`) must be
able to skip Vitals measurements on Well Child encounters. The Vitals form
in CHW mode (`VitalsFormBasic`) carries **two** measurements — respiratory
rate and body temperature — that must be skippable **independently**.

## Why a different design from PR #1653's

The encounter-level skip set works cleanly for single-measurement forms,
but creates a redundancy and a source-of-truth conflict for
multi-measurement forms with nullable value fields:

> Scenario: User saves Vitals with RR=20, Temp=37 → `well_child_vitals`
> row exists with both values. User re-edits, marks both as "unable to
> take", saves. With encounter-level skips, only the encounter's
> `skipped_forms` is updated; the existing measurement row is left alone.
> The encounter's set says "both skipped" while the row says "both have
> values" — two layers contradicting each other.

For Vitals — and for any future multi-measurement form — the cleaner
design is to push the skip indicator down onto the measurement node itself
via nullable value fields. The measurement node becomes the single source
of truth: a null field on a saved row means "we tried to take this
measurement but couldn't."

This spec adopts that approach **for Vitals only**. PR #1653's existing
`field_skipped_forms` mechanism for Height and Weight is unchanged.

## Non-goals

- **No changes to PR #1653's Height/Weight skip mechanism.** The
  `SkippedForm` type, the `field_skipped_forms` Drupal field, the
  encounter records' `skippedForms` field, and the
  `Add/RemoveSkippedForm` msgs all stay exactly as PR #1653 has them. We
  do not retroactively migrate Height/Weight to null-based skipping.
- No skip support for AcuteIllness CHW Vitals (also `VitalsFormBasic`
  but outside the requirement).
- No skip support for Vitals fields beyond RR / temp (BP, heart rate,
  repeated readings — never relevant in `VitalsFormBasic`).
- No progress-report UI distinguishing "skipped" from "missing."
  Consistent with PR #1653's Height/Weight precedent.
- No reporting/stats updates: the only server-side reads of these fields
  are on `acute_illness_vitals`, which is unaffected.
- No retroactive backfill of existing `well_child_vitals` rows.

## Architecture overview

```
                     CLIENT (Elm)                                 SERVER (Drupal)
┌───────────────────────────────────────────┐    ┌──────────────────────────────────────────┐
│ VitalsValue                               │    │ well_child_vitals (bundle)               │
│   { respiratoryRate : Maybe Int           │    │   field_respiratory_rate                 │
│   , bodyTemperature : Maybe Float         │    │     required => 0                        │
│   , ...other fields...                    │    │   field_body_temperature                 │
│   }                                       │ ◄──┤     required => 0                        │
└───────────────────────────────────────────┘    │                                          │
                                                 │ Other 3 Vitals bundles                   │
                                                 │ (vitals, acute_illness_vitals,           │
┌───────────────────────────────────────────┐    │  ncd_vitals): unchanged.                 │
│ Skip indication: a `well_child_vitals`    │    │ Their bundles' field_respiratory_rate /  │
│ row's null field IS the skip indicator.   │    │ field_body_temperature stay              │
│ Row exists, RR=null → CHW skipped RR.     │    │ required => 1.                           │
│ Row exists, RR=Just _ → CHW entered RR.   │    └──────────────────────────────────────────┘
│ Row never exists → form never saved.      │
└───────────────────────────────────────────┘

NO encounter-level field for Vitals skips. PR #1653's field_skipped_forms
on well_child_encounter and nutrition_encounter remains unchanged and
still serves Height/Weight.
```

**Single source of truth for Vitals skip state**: the `well_child_vitals`
node itself. The form's transient `*NotTaken` flags exist only to drive
UI rendering; on save, they translate to nulls in the persisted row.

## Naming

No new type, no rename. PR #1653's `SkippedForm = SkippedHeight |
SkippedWeight` stays untouched. The skip semantics for Vitals live in
the `VitalsValue` value type itself — no domain type for "vitals skip"
is introduced.

The form-side flags follow the convention PR #1653 set with
`HeightForm.measurementNotTaken : Maybe Bool`, but per measurement:
`VitalsForm.respiratoryRateNotTaken : Maybe Bool` and
`VitalsForm.bodyTemperatureNotTaken : Maybe Bool`. Same `Maybe Bool`
shape; per-measurement names because Vitals carries two skippable
measurements per form.

## Data model

### `VitalsValue` nullability

```elm
type alias VitalsValue =
    { sys : Maybe Float
    , dia : Maybe Float
    , heartRate : Maybe Int
    , respiratoryRate : Maybe Int   -- was Int
    , bodyTemperature : Maybe Float -- was Float
    , sysRepeated : Maybe Float
    , diaRepeated : Maybe Float
    }
```

Decoder: `optional "respiratory_rate" (nullable int) Nothing` and
analogous for body temperature. Encoder:
`( "respiratory_rate", maybe int v.respiratoryRate )`.

The four encounter types (`Vitals` for Prenatal, `AcuteIllnessVitals`,
`WellChildVitals`, `NCDVitals`) all alias `VitalsValue`. The decoder is
shared; the change is one place. Existing rows in any bundle have non-null
values at the DB level — the new decoder reads them as `Just _`. New
`Nothing` writes happen only on Well Child CHW Vitals saves.

### Drupal schema

Only `well_child_vitals`'s field instances need permissive (`required =>
0`) settings to accept null writes. As of the current branch HEAD, both
`node-well_child_vitals-field_respiratory_rate` and
`node-well_child_vitals-field_body_temperature` are already
`'required' => 0` in source (set in commit `bdfd30c97 "Introduce 2 new
CTs"`). No source change is required, only confirmation via `drush fra`
that the live DB matches.

`vitals` (Prenatal), `acute_illness_vitals`, and `ncd_vitals` field
instances keep `required => 1`. Server-side validation still rejects
null on those bundles. Their forms always populate values before save.

### Encounter records

`WellChildEncounter` and `NutritionEncounter` are **unchanged from PR
#1653**. The `skippedForms : EverySet SkippedForm` field stays. The
`Add/RemoveSkippedForm` msgs stay. Vitals does not interact with this
mechanism.

## Form/UX layer

### `VitalsForm`

```elm
type alias VitalsForm =
    { sysBloodPressure : Maybe Float
    , sysBloodPressureDirty : Bool
    , diaBloodPressure : Maybe Float
    , diaBloodPressureDirty : Bool
    , heartRate : Maybe Int
    , heartRateDirty : Bool
    , respiratoryRate : Maybe Int
    , respiratoryRateDirty : Bool
    , respiratoryRateNotTaken : Maybe Bool      -- NEW
    , bodyTemperature : Maybe Float
    , bodyTemperatureDirty : Bool
    , bodyTemperatureNotTaken : Maybe Bool      -- NEW
    , sysRepeated : Maybe Float
    , sysRepeatedDirty : Bool
    , diaRepeated : Maybe Float
    , diaRepeatedDirty : Bool
    }
```

Per-measurement transient flags. They drive the UI checkbox state and
get translated to null fields on the saved `VitalsValue` at save time.

### `VitalsFormConfig`

```elm
type alias VitalsFormConfig msg =
    { -- ... existing fields ...
    , setRespiratoryRateNotTakenMsg : Bool -> msg     -- NEW
    , setBodyTemperatureNotTakenMsg : Bool -> msg     -- NEW
    , allowSkipping : Bool                            -- NEW
    }
```

`allowSkipping` is a single boolean — both checkboxes appear together or
neither does. YAGNI on per-measurement gates: there is no requirement to
allow skipping one but not the other.

### `allowSkipping` is set per encounter type

| Page | Mode | `allowSkipping` |
|---|---|---|
| Prenatal | `VitalsFormFull` | `False` |
| NCD | `VitalsFormFull` | `False` |
| AcuteIllness (nurse) | `VitalsFormFull` | `False` |
| AcuteIllness (CHW) | `VitalsFormBasic` | `False` |
| WellChild | `VitalsFormBasic` | `isChw` |

`Pages/WellChild/Activity/Utils.elm` `generateVitalsFormConfig` gains
an `isChw : Bool` parameter (matching AcuteIllness's signature) so the
gate can be set without re-deriving from `assembled.encounter.encounterType`.

The other three pages (Prenatal, Prenatal Recurrent, NCD, AcuteIllness)
each get their `VitalsFormConfig` constructor updated with
`allowSkipping = False` and no-op msgs for `setRespiratoryRateNotTakenMsg`
and `setBodyTemperatureNotTakenMsg`. AcuteIllness gets a private
`IgnoreSkipMsg Bool` no-op msg added to its `Msg` type since it lacks
a `NoOp`; the others reuse existing `NoOp`.

### View rendering

In `vitalsFormInputsAndTasks` (`Measurement/View.elm`): when
`config.allowSkipping`, render an "Unable to take measurement" checkbox
under each of the RR and temp inputs. When a checkbox is checked, the
corresponding numeric input is hidden. Task tracker accepts the checked
box in lieu of a numeric value. The two checkboxes are independent.

`UnableToTakeMeasurement` translation already exists from PR #1653.

## Form ↔ value plumbing

### `vitalsFormWithDefault`

```elm
vitalsFormWithDefault : VitalsForm -> Maybe VitalsValue -> VitalsForm
vitalsFormWithDefault form saved =
    saved
        |> unwrap
            -- No saved value: use form as is.
            form
            (\value ->
                { -- ... blood pressure / heart rate / repeated fields, unchanged ...
                , respiratoryRate = maybeValueConsideringIsDirtyField form.respiratoryRateDirty form.respiratoryRate value.respiratoryRate
                , respiratoryRateDirty = form.respiratoryRateDirty
                , respiratoryRateNotTaken =
                    if form.respiratoryRateDirty then
                        form.respiratoryRateNotTaken

                    else
                        -- Derive from saved row: null = skipped.
                        case value.respiratoryRate of
                            Just _ ->
                                Just False

                            Nothing ->
                                Just True
                , bodyTemperature = maybeValueConsideringIsDirtyField form.bodyTemperatureDirty form.bodyTemperature value.bodyTemperature
                , bodyTemperatureDirty = form.bodyTemperatureDirty
                , bodyTemperatureNotTaken =
                    if form.bodyTemperatureDirty then
                        form.bodyTemperatureNotTaken

                    else
                        case value.bodyTemperature of
                            Just _ ->
                                Just False

                            Nothing ->
                                Just True
                -- ... other fields ...
                }
            )
```

Note: **no `EverySet SkippedMeasurement` parameter**. The form's
checkbox state is derived purely from the saved row's null fields. If
the row's RR is null, the form's `respiratoryRateNotTaken` defaults to
`Just True` (checkbox shows checked).

### `toVitalsValue` and `toVitalsValueWithDefault`

```elm
toVitalsValue : VitalsForm -> Maybe VitalsValue
toVitalsValue form =
    let
        rrField =
            if form.respiratoryRateNotTaken == Just True then
                Nothing

            else
                form.respiratoryRate

        tempField =
            if form.bodyTemperatureNotTaken == Just True then
                Nothing

            else
                form.bodyTemperature

        anyContent =
            isJust rrField
                || isJust tempField
                || form.respiratoryRateNotTaken == Just True
                || form.bodyTemperatureNotTaken == Just True
    in
    if anyContent then
        Just
            (VitalsValue form.sysBloodPressure
                form.diaBloodPressure
                form.heartRate
                rrField
                tempField
                form.sysRepeated
                form.diaRepeated
            )

    else
        Nothing


toVitalsValueWithDefault : Maybe VitalsValue -> VitalsForm -> Maybe VitalsValue
toVitalsValueWithDefault saved form =
    vitalsFormWithDefault form saved
        |> toVitalsValue
```

`toVitalsValue` returns `Just` whenever the user has interacted with at
least one of RR or temp (entered or marked skipped). The constructed
`VitalsValue` carries nulls for skipped fields. This means that for
**both-skipped**, `toVitalsValue` still returns `Just (VitalsValue ...
respiratoryRate=Nothing bodyTemperature=Nothing ...)` — a row IS saved,
with both relevant fields as null. There is no "no row saved" case for
Vitals on save.

For non-CHW pages, the form always populates real values before save
(the form's task tracker enforces it), so `toVitalsValue` returns `Just`
with both fields populated.

## Save flow

`Pages/WellChild/Activity/Update.elm`. The `SaveVitals` msg has its
existing PR #1653 signature (no `EverySet SkippedMeasurement` parameter):

```elm
SaveVitals PersonId (Maybe ( WellChildVitalsId, WellChildVitals )) (Maybe DangerSignsTask)
```

Handler:

```elm
SaveVitals personId saved nextTask ->
    let
        form_ =
            model.dangerSignsData.vitalsForm

        measurementId =
            Maybe.map Tuple.first saved

        measurement =
            getMeasurementValueFunc saved

        appMsgs =
            toVitalsValueWithDefault measurement form_
                |> Maybe.map
                    (\value ->
                        [ Backend.WellChildEncounter.Model.SaveVitals personId measurementId value
                            |> Backend.Model.MsgWellChildEncounter id
                            |> App.Model.MsgIndexedDb
                        ]
                    )
                |> Maybe.withDefault []

        extraMsgs =
            generateDangerSignsMsgs nextTask
    in
    ( model, Cmd.none, appMsgs )
        |> sequenceExtra (update currentDate site id db) extraMsgs
```

Single code path. No `if rrSkip && tempSkip then ...` branching. No
`Add/RemoveSkippedMeasurement` msgs (because we don't have those —
PR #1653's encounter-level skip set is unchanged and only used for
Height/Weight).

The constructed `VitalsValue` carries the skip semantics via its null
fields. The save dispatches a single PATCH/POST to the
`well_child_vitals` shard endpoint, which the service worker intercepts
and writes to local IndexedDB plus the `shardChanges` outgoing queue.
On reopen, the form rehydrates from that row's null fields.

## `Maybe` propagation through read-sites

Phase 2 of the prior branch already audited these sites; the work
carries over identically. Full list:

| Site | Pattern | Change |
|---|---|---|
| `Backend/Update.elm:2342-2350` | `value.bodyTemperature < 35 \|\| value.bodyTemperature >= 37.5` (Well Child danger signs) | Wrap each comparison in `Maybe.map (\t -> ...) \|> Maybe.withDefault False`. Semantic: missing measurement → no alert. |
| `Backend/PrenatalActivity/Utils.elm:280,404` | `value \|> .bodyTemperature` / `.respiratoryRate` inside an outer `Maybe.andThen` | Inner becomes nested `Maybe.andThen`. |
| `Pages/AcuteIllness/ProgressReport/View.elm:466,489` | `Maybe.map .bodyTemperature` | Becomes `Maybe.andThen .bodyTemperature` to flatten. |
| `Pages/AcuteIllness/Activity/Utils.elm:2723` (`feverAtPhysicalExam`) | `.bodyTemperature` direct read inside `Maybe.andThen` | `Maybe.andThen` chain on the inner field. |
| `Pages/AcuteIllness/Activity/Utils.elm:2744,2763` (`respiratoryRateElevated`, `respiratoryRateElevatedForCovid19`) | `.respiratoryRate` direct read inside `Maybe.map` | `Maybe.andThen` and `Maybe.withDefault False`. |
| `Pages/AcuteIllness/Activity/View.elm:180,191` | Point-free `(.bodyTemperature ...)` / `(.respiratoryRate ...)` accessors inside `Maybe.map` chains | Replace with `Maybe.andThen`. |
| `Pages/Prenatal/Activity/Utils.elm:3418` | `Maybe.map (\value -> value.respiratoryRate > 30)` | Nested `Maybe.andThen` to flatten the now-`Maybe Int`. |
| Four `resolvePreviousValue ... .vitals .respiratoryRate` / `.bodyTemperature` sites in WellChild, Prenatal, NCD, AcuteIllness `Activity/Utils.elm` | Passes field accessor as `(a -> b)` argument | Switch to existing `resolvePreviousMaybeValue` helper for Prenatal/NCD/AcuteIllness; for WellChild use `\|> Maybe.andThen identity` to flatten. |
| `Measurement/Utils.elm` `vitalsFormWithDefault`, `toVitalsValueWithDefault`, `toVitalsValue` | Form ↔ value plumbing | See "Form ↔ value plumbing" section above. |
| `Backend/Measurement/Decoder.elm` `decodeVitalsValue` | `required "respiratory_rate" int` / `"body_temperature" float` | Switch to `optional ... (nullable int) Nothing`. |
| `Backend/Measurement/Encoder.elm` `encodeVitalsValueWithType` | `( "respiratory_rate", int v.respiratoryRate )` | `( "respiratory_rate", maybe int v.respiratoryRate )`. |

### Helper-signature stance

Functions like `respiratoryRateElevatedByAge : Maybe Int -> Int -> Bool`
and `respiratoryRateAbnormalForAge` keep their `Int` second parameter.
Callers do the `Maybe.andThen`. Rationale: helpers express clinical
predicates on a *known* rate; "we have no rate" is a caller-side decision.

## End-encounter gating

Today, `Pages/WellChild/Activity/Utils.elm:727-740` has the implicit
optionality pattern PR #1653 was designed to replace:

```elm
resolvedMandatoryDangerSignsTasksCompleted site assembled =
    case assembled.encounter.encounterType of
        PediatricCare ->
            [ TaskSymptomsReview, TaskVitals ]
        _ ->
            if site == SiteBurundi then
                [ TaskSymptomsReview ]   -- Vitals silently optional
            else
                [ TaskSymptomsReview, TaskVitals ]
```

Burundi CHWs can currently end Well Child encounters without ever
opening Vitals.

### Changes

1. `TaskVitals` becomes always mandatory:

   ```elm
   resolvedMandatoryDangerSignsTasksCompleted assembled =
       case assembled.encounter.encounterType of
           PediatricCare ->
               [ TaskSymptomsReview, TaskVitals ]
           _ ->
               [ TaskSymptomsReview, TaskVitals ]
   ```

2. `dangerSignsTaskCompleted` for `TaskVitals` returns to the simple
   form: a row exists.

   ```elm
   TaskVitals ->
       isJust measurements.vitals
   ```

   The row's existence is the proof of "task done" because the row
   carries the skip state via its null fields. If the row exists with
   one or both fields null, the user explicitly chose to skip those.

3. `Site` parameter ripples out:
   - `mandatoryDangerSignsTasksCompleted : Site -> AssembledData -> Bool`
     → `AssembledData -> Bool`.
   - `allowEndingEncounter : NominalDate -> Site -> List WellChildActivity -> AssembledData -> Bool`
     → `NominalDate -> List WellChildActivity -> AssembledData -> Bool`.
   - Callsites in `Pages/WellChild/Encounter/View.elm:241`,
     `Pages/WellChild/Encounter/Utils.elm:147,154,161-162,169-170`,
     `Pages/WellChild/Activity/Utils.elm:1550` lose the `site` argument.

After this change, `Site` no longer appears in mandatory-task logic.
The only remaining use of role/site for skipping is the `allowSkipping`
flag in `generateVitalsFormConfig` — UI policy, not data semantics.

## E2E coverage

Extend `client/e2e/helpers/well-child.ts` with two helpers:

- `skipRespiratoryRate(page)` — open Vitals tab, click the RR
  "Unable to take measurement" checkbox.
- `skipBodyTemperature(page)` — same for temp.

The helpers do **not** include a Save action — the existing test will
call Save once both fields are resolved (entered or skipped).

Add three test variants to `client/e2e/well-child-encounter-chw.spec.ts`:

1. **Partial skip — RR skipped, temp entered.** CHW Well Child encounter
   with all mandatory activities completed. For Vitals: call
   `skipRespiratoryRate(page)`, fill body temperature normally, save.
   Encounter ends successfully. Backend assertion: a `well_child_vitals`
   row exists for the encounter, with `field_respiratory_rate` NULL and
   `field_body_temperature` populated.

2. **Both skipped.** Same setup. For Vitals: call both
   `skipRespiratoryRate(page)` and `skipBodyTemperature(page)`, save.
   Encounter ends successfully. Backend assertion: a `well_child_vitals`
   row exists with both `field_respiratory_rate` and
   `field_body_temperature` NULL.

3. **Negative gate — PediatricCare nurse OR AcuteIllness CHW.**
   No skip checkboxes appear. (Pick whichever role has an existing
   e2e flow that's quickest to extend.)

Existing tests for non-skip CHW Well Child Vitals (where both values
are entered) must continue to pass without modification.

## Branching plan

**Base:** `revert-1641-restore-issue-1448` (PR #1653, untouched).

**Working branch:** `vitals-skip-nulls`, branched off the current HEAD
of `revert-1641-restore-issue-1448` (commit `c0ad16351`).

**Workflow:**
1. Land the commits below on `vitals-skip-nulls`.
2. Open a PR targeting `revert-1641-restore-issue-1448` (NOT `develop`).
3. After approval, merge into `revert-1641-restore-issue-1448`.
4. PR #1653 remains the single delivery vehicle to `develop` and
   absorbs the Vitals work via this merge.

`develop` only ever sees the final, coherent state when #1653 lands.

## Commit structure

Three commits, each independently compiles and passes existing tests.

1. **Make `VitalsValue.respiratoryRate` and `.bodyTemperature` nullable;
   propagate `Maybe` through read-sites.** Touches:
   `Backend/Measurement/Model.elm`, `Backend/Measurement/Decoder.elm`,
   `Backend/Measurement/Encoder.elm`, the read-sites listed above,
   `Measurement/Utils.elm` `vitalsFormWithDefault` /
   `toVitalsValue` / `toVitalsValueWithDefault`. No behavior change yet —
   non-CHW pages always populate both fields; the form skip checkboxes
   come in commit 2. Drupal: confirm `well_child_vitals` field instances
   are already `required => 0` (no source change needed).

2. **Add Vitals skip UX and end-encounter gating cleanup.** Touches:
   `Measurement/Model.elm` (`VitalsForm` per-measurement flags;
   `VitalsFormConfig.allowSkipping` plus the two `set*NotTakenMsg`
   callbacks), `Measurement/View.elm` (checkbox rendering),
   `Pages/WellChild/Activity/{Model,Update,View,Utils}.elm`
   (`SetRespiratoryRateNotTaken` / `SetBodyTemperatureNotTaken` msgs,
   `generateVitalsFormConfig isChw`, drop `Site` from mandatory-tasks),
   `Pages/WellChild/Encounter/{View,Utils}.elm` (drop `Site` from
   `allowEndingEncounter`), other `generateVitalsFormConfig` callers
   (Prenatal, Prenatal Recurrent, NCD, AcuteIllness — set
   `allowSkipping = False`; AcuteIllness gains a private
   `IgnoreSkipMsg` no-op).

3. **E2E helpers and tests** for Vitals skipping.

## Risk register

**Risks accepted (consistent with PR #1653's precedent for Height/Weight)**

- Save dispatches PATCH/POST asynchronously through the service worker.
  If the service worker fails to write to IndexedDB (offline edge,
  storage quota), the save fails. UI doesn't currently surface this.
  Same as every other measurement save in the app.

**Risks specific to this branching strategy**

- PR #1653 may receive review feedback that conflicts with this branch's
  changes. Mitigation: rebase `vitals-skip-nulls` onto updated
  `revert-1641-restore-issue-1448` periodically; merge before any major
  #1653 churn.

## Success criteria

- Any CHW (Burundi or Rwanda in practice) can open the Vitals tab in a
  Well Child encounter, mark either or both of RR / temp as "Unable to
  take measurement," and end the encounter successfully.
- A Well Child PediatricCare nurse encounter does not show the
  checkboxes.
- AcuteIllness CHW encounters (also `VitalsFormBasic`) do not show
  the checkboxes — page-level gate, not mode-derived.
- Save always writes a `well_child_vitals` row when the form is saved
  (regardless of skip state). Skipped fields are persisted as null.
- Reopening the form rehydrates the checkbox state from the saved
  row's null fields. Refreshing the browser (which re-reads IndexedDB)
  preserves the state once the service worker has written the row.
- Activity-completion logic for `TaskVitals` is `isJust measurements.vitals`.
  Encounter-end logic does not branch on `Site`.
- Existing AcuteIllness, Prenatal, NCD Vitals flows are unchanged from
  a user-visible perspective.
- PR #1653's Height/Weight `field_skipped_forms` mechanism is untouched
  and continues to work as before.
- Lint (`elm-format`, `phpcs`) clean.

## What this design abandons from the prior branch

For traceability, listing what's **explicitly not in this design** that
was on `vitals-skip-measurement`:

- `SkippedForm → SkippedMeasurement` rename — reverted; `SkippedForm`
  stays.
- New `SkippedRespiratoryRate` / `SkippedBodyTemperature` variants —
  not added.
- Drupal `field_skipped_forms → field_skipped_measurements` rename —
  reverted; field name stays `field_skipped_forms`.
- Drupal `allowed_values` expansion to four entries — reverted; stays
  `[height, weight]`.
- `hedley_schedule_update_7001` / `_7002` install hooks — not added.
- `Add/RemoveSkippedMeasurement` msgs in `Backend.WellChildEncounter` —
  not added (the existing PR #1653 msgs `Add/RemoveSkippedForm` stay).
- Encounter-level `skippedMeasurements` field — not added; PR #1653's
  `skippedForms` stays.
- Optimistic encounter Dict update in `Backend.Update.elm` — not added.
- `vitalsFormWithDefault` taking `EverySet SkippedMeasurement` — does
  not take that parameter; reads skip state from the saved row's nulls.

The `vitals-skip-measurement` branch is preserved but not delivered.
