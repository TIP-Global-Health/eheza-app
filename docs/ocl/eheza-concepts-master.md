This file documents `docs/ocl/eheza-concepts-master.csv` — a canonical
inventory of every concept E-Heza captures, drawn from a deterministic walk
of 36 of the 40 `client/src/elm/Backend/*/Model.elm` files plus the 3
`Backend/*/Types.elm` files (`AcuteIllnessEncounter`, `NCDEncounter`,
`PrenatalEncounter`) that hold program-specific diagnosis enums. The 4
excluded Model.elm modules are `Dashboard`, `ResilienceMessage`,
`ResilienceSurvey`, and `PatientRecord` — UI rendering models, staff-
resilience HR data, and the PatientRecord composite view. The master's purpose is to be a search
baseline for future cross-organisation mapping passes (UVL-Burundi is a
known upcoming consumer; the architecture is generic for any further org).
It has its own `EHEZA-NNNN` ID space and is intentionally separate from the
existing per-encounter `<x>-concepts.csv` / `<x>-mappings.csv` files — the
two artefacts are not cross-referenced.

## Schema

| Column | Description | Example values |
|---|---|---|
| `id` | `EHEZA-NNNN`, zero-padded to 4 digits, sequentially assigned in walk order | `EHEZA-0001`, `EHEZA-1247` |
| `name` | English concept name. Sourced from `Translate.elm` when the Elm construct's name matches a `TranslationId`; otherwise a mechanical CamelCase/camelCase split of the identifier | `Symptom Review`, `Heart Rate`, `Sequence Number` |
| `kinyarwanda` | Kinyarwanda translation from `Translate.elm`, blank when there is no match or the locale is `Nothing` | `Kureba ibimenyetso by'uburwayi` |
| `kirundi` | Kirundi translation, same rule | `Kuraba ibimenyetso` |
| `somali` | Somali translation, same rule | `Eegis Calaamadaha ah` |
| `name_source` | `translation` if `name` came from `Translate.elm`; `fallback` if it came from the identifier split | `translation`, `fallback` |
| `translation_id` | The matched `TranslationId` constructor (e.g., `Heartburn` or `HeartburnLabel`); blank for fallback rows | `AcuteIllnessSymptoms`, `HeartburnLabel` |
| `eheza_field_path` | Full Elm reference: `<TypeName>` for types, `<TypeName>.<fieldName>` for record fields, `<UnionType>.<Constructor>` for union constructors | `BasicVitalsValue`, `BasicVitalsValue.sys`, `BreastfeedingSign.BreastfeedingExclusively` |
| `source_module` | Path to the Elm file the concept was found in, repo-relative | `client/src/elm/Backend/Measurement/Model.elm` |
| `elm_construct` | One of `record_type` / `record_field` / `union_type` / `union_constructor` | `record_field` |
| `datatype` | OpenMRS-flavoured datatype derived heuristically from the Elm type. See *Datatype derivation rules* below | `Numeric`, `Text`, `Coded`, `Boolean`, `Date`, `N/A` |

Columns intentionally **NOT** present, with rationale:

- `concept_class` — assigning OpenMRS concept classes (Question / Finding /
  Diagnosis / Misc / etc.) requires clinical judgment per row, and the right
  class often depends on which downstream dictionary the concept will be
  mapped into. Defer until OCL upload time.
- `description` — free-text descriptions are an editorial deliverable, not an
  inventory deliverable. Writing them now risks stale prose if the Elm field
  evolves before any consumer cares.
- `eheza_category` — categorisation by clinical domain (vitals / nutrition /
  obstetric / etc.) is a labelling exercise that is only useful for the
  consumer doing the cross-org mapping; that consumer can group rows on
  `source_module` instead, which is mechanical.
- `confidence` — the master is a pure structural extraction. There is nothing
  to be confident or unconfident about; confidence is a cross-mapping concept,
  not an inventory concept.

The master is an inventory, not a curated dictionary; consumers that need
clinical metadata layer it on at mapping time.

## Datatype derivation rules

| Elm type | `datatype` |
|---|---|
| `Int`, `Float`, `EverySet … Float` | `Numeric` |
| `String`, `EverySet String` | `Text` |
| `Bool` | `Boolean` |
| `NominalDate`, `Date` | `Date` |
| Custom union type (e.g., `BreastfeedingSign`) | `Coded` |
| `Maybe X` | datatype of `X` (nullability implicit) |
| Record types and union constructors themselves (the type / constructor row, not a field of it) | `N/A` |
| Union types themselves (the type row, not a constructor of it) | `Coded` (the type IS a coded question) |
| Anything else (nested records, custom EverySets of records, exotic Dicts, `Json.Decode.Value`) | `N/A` |

## Walk methodology

### Walk order

1. Files: 39 in-scope source files (36 `Backend/*/Model.elm` + 3 `Backend/*/Types.elm`), alphabetical by repo-relative path. For directories that have both `Model.elm` and `Types.elm` (AcuteIllnessEncounter, NCDEncounter, PrenatalEncounter), Model.elm is walked first.
2. Within a file: top-level type declarations in source order (top of file → bottom).
3. Within a declaration:
   - Record type `type alias Foo = { ... }`: mint `Foo` as `record_type`, then each field in source order as `record_field` with `eheza_field_path = Foo.fieldName`.
   - Union type `type Foo = A | B C | D ...`: mint `Foo` as `union_type`, then each constructor in source order as `union_constructor` with `eheza_field_path = Foo.A` (constructor argument types are implicit — not enumerated separately).
   - Non-record type alias `type alias FooId = EntityId Foo` or `type alias BarMap = Dict X Y`: mint `Foo` as `record_type` with `datatype = N/A`. No member rows.

### Name resolution (English + locales)

For each row, the walker looks up the bare identifier (`<TypeName>` for type
rows, `<fieldName>` for record fields, `<ConstructorName>` for union
constructors) in a resolution map built from `Translate.elm`'s
`translationSet` function:

1. **Exact match** — try `<name>` against the resolution map.
2. **Label fallback** — if step 1 misses, try `<name>Label` (matches the
   convention used to escape namespace clashes, e.g., `Heartburn` →
   `HeartburnLabel`).
3. **Identifier split** — if both miss, derive English mechanically:
   `heartRate` → `Heart Rate`, `BreathsPerMinute` → `Breaths Per Minute`.
   `kinyarwanda` / `kirundi` / `somali` are blank for fallback rows.

The resolution map covers both top-level zero-arg `translationSet` case
branches and zero-arg inner case branches inside arg-taking outer wrappers
(`OuterCtor x -> case x of`). Single-redirect bodies of the form
`translationSet OtherCtor` are followed transitively. First-write-wins on
inner-name collisions across different outer wrappers.

`translation_id` records which `TranslationId` was matched (the bare
constructor name, including any `Label` suffix). Empty when the row used the
identifier-split fallback.

### Phrase / question filter

Translation matches whose English reads as a UI question or sentence
fragment are dropped — the matching `TranslationId` exists but the string
itself is a dialog prompt, not a concept name. The filter triggers on
sentence-leading word patterns:

- *Question starters*: `Can`, `Could`, `Do`, `Does`, `Did`, `Are`, `Is`,
  `Has`, `Have`, `Was`, `Were`, `Will`, `Would`, `Should`, `What`, `When`,
  `Where`, `Why`, `How`, `Who`, `Which`, `On which`, `By which`.
- *Imperative / instructional*: `Give`, `Send`, `Provide`, `Advise`,
  `Advised`, `advised`, `agreed`, `Not dispensing`, `Not`.
- *First-person Likert / narrative*: `I`, `My`, `We`, `Things have`.
- *Third-person sentence narratives*: `The thought`, `There are`,
  `The Tetanus`, `Prevents`, `Protects`, `OPV`, `BCG`, `HPV`, `MR`,
  `Patient experienced`.
- *Dosing instructions*: `1 tablet`, `2 tablet`, `2 capsule`, `3 tablet`,
  `4 tabs`, `by mouth`, `IM`.
- *Severity / response*: `Severe Malaria`, `No response`.

The filter applies **only to translation rows**. Fallback rows (those whose
name was derived by identifier split) are exempt — a struct field name like
`HasCleanWater` produces `"Has Clean Water"`, which is a Boolean concept
even though it reads as a phrase.

### Deduplication

After name resolution and the phrase/question filter, rows whose `name`
collides under `name.strip().lower()` are collapsed to the first occurrence
(deterministic walk-order tiebreak). The dropped rows' `eheza_field_path`
and `source_module` are *not* preserved — consumers that need full
provenance should grep the codebase. This is the trade-off chosen to keep
the master a clean name dictionary.

### Cross-master mirror drop

After dedup, the walker reads `docs/ocl/master-drop-names.txt` and removes
any row whose `name` (case+whitespace-insensitive) matches an entry in that
file. The list is derived from the labels-master hand-review
(`labels-master-drop-tids.txt`): for each labels-dropped tid whose original
english is no longer present in the current labels master, the english is
recorded as a struct-master drop name. This keeps the two artefacts aligned
— if the curator decided a name isn't a real concept on the labels side, it
shouldn't reappear on the struct side.

### ID assignment

Sequential, starting at `EHEZA-0001`, incrementing by 1 per row. **Once the
file is committed, IDs are immutable.** If a future Elm change adds a new
type, it gets the next available `EHEZA-NNNN` regardless of where it falls in
walk order — the walk-order rule only governs the initial inventory pass.

### External references

- When a record field has a type defined outside the 36 in-scope modules (e.g., `Gizra.NominalDate.NominalDate`, `EntityId X`, `Json.Decode.Value`): apply the datatype derivation rule from above; do not follow the reference; field gets one row but the foreign type does not.
- When a record field references an in-scope-but-excluded module (e.g., `Backend.Dashboard.Model.SomeType`): field gets a row with `datatype = N/A`; the excluded type does not get a row.

### Edge cases

- **Phantom-type / zero-argument union constructors**: treat exactly like multi-arg constructors — one `union_constructor` row.
- **Recursive types** (`type Tree = Leaf | Node Tree Tree`): mint the type and all constructors; recursion in the *type signature* doesn't produce extra rows.
- **Functions and operators inside `Model.elm`** (rare but they exist for utility helpers): not concepts; skipped.
- **Type signatures without declarations** (`port`, `effect`, etc., if they appear in `Model.elm`): skipped.
- **Comments and module-level docstrings**: skipped.
- **Re-exports** (`module Foo exposing (Bar)` where `Bar` is defined elsewhere): the row is emitted only in the file where the type is *declared*, not where it is re-exposed. The `source_module` column always points at the declaration site.
- **Empty record types** (`type alias Foo = {}`, if any exist): mint the type row; no field rows. The `datatype` of the type row is `N/A` per the rule above.

## Inventory pass metadata

- **Walk date**: `2026-04-20`
- **Source tree SHA**: latest commit on `docs/eheza-concepts-master`
- **Walker tool**: scratch Python script at `/tmp/eheza-master-rebuild.py` (not committed)
- **Raw walker rows**: 3566 (39 files; +143 vs the 3423 from the Model.elm-only walk)
- **Post-walk filter**: 635 Elm-Architecture `Msg` / `Model` framework rows dropped (`eheza_field_path == 'Msg' | 'Model'` or starts with `Msg.` / `Model.`). Net 2931 rows after filter.
- **Phrase/question drops**: 246 translation rows dropped because the matched English read as a UI question, sentence fragment, or instructional step (see *Phrase / question filter* above).
- **Translation matches**: 738 rows have `name_source = translation` (English + 0–3 non-English locales sourced from `Translate.elm`).
- **Fallback names**: 1139 rows used identifier-split fallback (English-only).
- **Duplicates collapsed**: 774 rows dropped during dedup (case+whitespace-insensitive `name` collision; first occurrence wins). The dedup count is higher than a single-pass walk because dispatch chains in `Translate.elm` (e.g., `DiagnosisChronicHypertensionAfterRecheck → DiagnosisChronicHypertensionImmediate`) cause clinically-equivalent variants to resolve to the same English label and collapse together.
- **Cross-master mirror drops** (`master-drop-names.txt`): 116 rows dropped because their `name` matched an english value that was hand-removed from the labels master (mirrors the labels curation so the two artefacts stay aligned at the concept-name level).
- **Final row count**: 1761

*Build process is hand-driven and one-shot per the spec; no walker script
lives in the repo. The scratch tool used for the initial pass is documented
in the implementation plan but not preserved.*

## Process discipline for future Elm changes

> If you add a new measurement type alias, new record field, or new union
> constructor to an in-scope module (any of the 36 listed in *Scope*), append
> a row to `eheza-concepts-master.csv` with the next available `EHEZA-NNNN`
> id as part of the same PR. The walk order is only used to assign IDs during
> the *initial* inventory pass; net-new entries after that get the next free
> id regardless of where they fall in walk order.
>
> If the new entry would collide with an existing row's `name` (case +
> whitespace-insensitive), do **not** append a duplicate; treat the existing
> row as authoritative.

## Scope

**In scope** — 36 `Backend/*/Model.elm` files, grouped by category for
readability:

- *Clinical core*: `Measurement` plus the 10 `<Encounter>Activity` directories — `AcuteIllnessActivity`, `ChildScoreboardActivity`, `FamilyNutritionActivity`, `HIVActivity`, `HomeVisitActivity`, `NCDActivity`, `NutritionActivity`, `PrenatalActivity`, `TuberculosisActivity`, `WellChildActivity`.
- *Person*: `Person`.
- *Encounter scaffolding*: the 10 `<Encounter>Encounter` directories — `AcuteIllnessEncounter`, `ChildScoreboardEncounter`, `FamilyNutritionEncounter`, `HIVEncounter`, `HomeVisitEncounter`, `NCDEncounter`, `NutritionEncounter`, `PrenatalEncounter`, `TuberculosisEncounter`, `WellChildEncounter`.
- *Care-delivery operational*: `HealthCenter`, `Village`, `Clinic`, `Nurse`, `Session`, `ParticipantConsent`, `Relationship`, `Counseling`, `EducationSession`, `TraceContact`, `StockUpdate`, `PmtctParticipant`, `IndividualEncounterParticipant`, `FamilyEncounterParticipant`.

**Out of scope** — 4 excluded modules:

- `Dashboard` — UI rendering of dashboard tiles. The types in this module
  describe the shape of pre-aggregated data displayed to nurses, not facts
  captured at the point of care. Including them would inflate the inventory
  with derived/projected fields that have no upstream concept.
- `ResilienceMessage` — staff-resilience HR messaging. Internal workforce
  support content, not patient data.
- `ResilienceSurvey` — staff-resilience HR survey responses. Same reasoning
  as above; staff well-being instrumentation, not clinical capture.
- `PatientRecord` — UI composite view that re-projects data already
  represented in the encounter and measurement modules. Including it would
  produce duplicate rows for the same underlying concepts.

Reopening this decision is out of scope for this pass; a separate brainstorm
if anyone wants to revisit.

## Explicit non-relationship to per-encounter CSVs

Master and per-encounter CSV files have **separate ID spaces, separate
purposes, no cross-references**. Specifically:

- The master file is NOT updated when a per-encounter PIH mapping changes.
- The per-encounter files are NOT updated when the master gains rows.
- Future cross-organisation mapping passes consume the master, NOT the per-encounter files.
- Tools that operate on the master (search, diffing, cross-org mapping) do not need to read the per-encounter files at all, and vice versa.

This is intentional — it keeps the live `TIP-Global-Health/EHEZA` OCL source
stable and lets the master evolve independently as the Elm codebase grows.
The two artefacts answer different questions: the per-encounter files answer
"what did we map to PIH for encounter X", while the master answers "what
concepts does E-Heza capture, full stop". Conflating them would force every
PIH-mapping change to ripple into the master, and every Elm-shape change to
ripple into the per-encounter files, which is the maintenance pattern this
separation is designed to avoid.
