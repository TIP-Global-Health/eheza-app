This file documents `docs/ocl/eheza-concepts-master.csv` — a canonical inventory
of every concept E-Heza captures, drawn from a deterministic walk of 37 of the
40 `client/src/elm/Backend/*/Model.elm` files. The 4 excluded modules are
`Dashboard`, `ResilienceMessage`, `ResilienceSurvey`, and `PatientRecord` — UI
rendering models, staff-resilience HR data, and the PatientRecord composite
view. The master's purpose is to be a search baseline for future
cross-organisation mapping passes (UVL-Burundi is a known upcoming consumer;
the architecture is generic for any further org). It has its own `EHEZA-NNNN`
ID space and is intentionally separate from the existing per-encounter
`<x>-concepts.csv` / `<x>-mappings.csv` files — the two artefacts are not
cross-referenced.

## Schema

| Column | Description | Example values |
|---|---|---|
| `id` | `EHEZA-NNNN`, zero-padded to 4 digits, sequentially assigned in walk order | `EHEZA-0001`, `EHEZA-1247` |
| `name` | The Elm identifier, lightly prettified (CamelCase split on case boundaries, lowercased except first letter). Pure mechanical transform — no clinical interpretation | `Basic vitals value`, `Breastfeeding sign breastfeeding exclusively`, `Sys` |
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

1. Files: 37 in-scope source files, alphabetical by repo-relative path.
2. Within a file: top-level type declarations in source order (top of file → bottom).
3. Within a declaration:
   - Record type `type alias Foo = { ... }`: mint `Foo` as `record_type`, then each field in source order as `record_field` with `eheza_field_path = Foo.fieldName`.
   - Union type `type Foo = A | B C | D ...`: mint `Foo` as `union_type`, then each constructor in source order as `union_constructor` with `eheza_field_path = Foo.A` (constructor argument types are implicit — not enumerated separately).
   - Non-record type alias `type alias FooId = EntityId Foo` or `type alias BarMap = Dict X Y`: mint `Foo` as `record_type` with `datatype = N/A`. No member rows.

### ID assignment

Sequential, starting at `EHEZA-0001`, incrementing by 1 per row. **Once the
file is committed, IDs are immutable.** If a future Elm change adds a new
type, it gets the next available `EHEZA-NNNN` regardless of where it falls in
walk order — the walk-order rule only governs the initial inventory pass.

### External references

- When a record field has a type defined outside the 37 in-scope modules (e.g., `Gizra.NominalDate.NominalDate`, `EntityId X`, `Json.Decode.Value`): apply the datatype derivation rule from above; do not follow the reference; field gets one row but the foreign type does not.
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

- **Walk date**: `<YYYY-MM-DD>`
- **Source tree SHA**: `<git rev-parse HEAD output>`
- **Walker tool**: scratch Python script at `/tmp/eheza-walker.py` (not committed; see *Build process* note below)
- **Initial row count**: `<filled in at CSV commit>`

*Build process is hand-driven and one-shot per the spec; no walker script
lives in the repo. The scratch tool used for the initial pass is documented
in the implementation plan but not preserved.*

## Process discipline for future Elm changes

> If you add a new measurement type alias, new record field, or new union
> constructor to an in-scope module (any of the 37 listed in *Scope*), append
> a row to `eheza-concepts-master.csv` with the next available `EHEZA-NNNN`
> id as part of the same PR. The walk order is only used to assign IDs during
> the *initial* inventory pass; net-new entries after that get the next free
> id regardless of where they fall in walk order.

## Scope

**In scope** — 37 `Backend/*/Model.elm` files, grouped by category for
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
