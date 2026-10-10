# E-Heza concepts master inventory — design

Builds a single canonical inventory of every clinical / patient-registration
/ care-delivery concept E-Heza captures, drawn from a deterministic walk
of the in-scope `Backend/*/Model.elm` files. The deliverable is one new CSV
(`docs/ocl/eheza-concepts-master.csv`) plus a methodology doc and a README
update — purely local artefacts; no OCL upload in this pass and no edits
to the existing per-encounter `<x>-concepts.csv` / `<x>-mappings.csv`
files. The inventory exists to be the search baseline for future
external-organisation mapping passes (UVL-Burundi is a known upcoming
consumer; the architecture is generic so any further org can use it the
same way).

The methodology inverts the previous PIH-driven workflow: instead of
"search PIH → mint concept on a hit", the future flow becomes "enumerate
every E-Heza concept → for each target org, search and add SAME-AS where
possible". This pass produces only the enumeration; mapping is left to
later passes.

## Scope

In: every concept in 37 of the 40 `client/src/elm/Backend/*/Model.elm`
files. Excluded: `Dashboard`, `ResilienceMessage`, `ResilienceSurvey`,
`PatientRecord` (UI rendering models, staff-resilience HR data, and the
PatientRecord composite view — none of which are clinical concepts).

The 37 in-scope files break down as:
- **Clinical core**: `Measurement` + the 10 `<Encounter>Activity` directories
- **Person**: `Person`
- **Encounter scaffolding**: the 10 `<Encounter>Encounter` directories (encounter dates, status, diagnosis enums)
- **Care-delivery operational**: `HealthCenter`, `Village`, `Clinic`, `Nurse`, `Session`, `ParticipantConsent`, `Relationship`, `Counseling`, `EducationSession`, `TraceContact`, `StockUpdate`, `PmtctParticipant`, `IndividualEncounterParticipant`, `FamilyEncounterParticipant`

Total estimated rows: **~1000–1500**.

**ID space:** every row gets a fresh `EHEZA-NNNN` id (zero-padded to 4
digits, sequential). This includes the 217 existing PIH-mapped concepts
that already carry `EH-<PREFIX>-NNN` ids in the per-encounter CSVs —
they're enumerated again in the master with new `EHEZA-NNNN` ids and
**no breadcrumb** back to their existing PIH-mapped identity. The master
treats the prior PIH work as if it never existed; the two artefacts
coexist with separate ID spaces and no cross-pollination.

This pass does NOT:
- Upload anything to OCL (deferred — happens when the first cross-org
  mapping pass needs the inventory live)
- Mint mappings to PIH or any other org (mappings stay in existing
  per-encounter files; UVL is future)
- Edit existing `<x>-concepts.csv` / `<x>-mappings.csv` files
- Commit a regenerator script (one-shot manual inventory; future Elm
  changes append rows by hand as part of the same PR)

## Schema

`docs/ocl/eheza-concepts-master.csv` — strict CSV (RFC 4180), UTF-8,
6 columns:

| Column | Description | Example values |
|---|---|---|
| `id` | `EHEZA-NNNN`, zero-padded to 4 digits, sequentially assigned in walk order | `EHEZA-0001`, `EHEZA-1247` |
| `name` | The Elm identifier, lightly prettified (CamelCase split on case boundaries, lowercased except first letter). Pure mechanical transform — no clinical interpretation | `Basic vitals value`, `Breastfeeding sign breastfeeding exclusively`, `Sys` |
| `eheza_field_path` | Full Elm reference: `<TypeName>` for types, `<TypeName>.<fieldName>` for record fields, `<UnionType>.<Constructor>` for union constructors | `BasicVitalsValue`, `BasicVitalsValue.sys`, `BreastfeedingSign.BreastfeedingExclusively` |
| `source_module` | Path to the Elm file the concept was found in, repo-relative | `client/src/elm/Backend/Measurement/Model.elm` |
| `elm_construct` | One of `record_type` / `record_field` / `union_type` / `union_constructor` | `record_field` |
| `datatype` | OpenMRS-flavoured datatype derived heuristically from the Elm type. See rules below | `Numeric`, `Text`, `Coded`, `Boolean`, `Date`, `N/A` |

**Datatype derivation rules** (mechanical; reproduced in the methodology
doc):

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

**Columns intentionally NOT in the schema:** `concept_class`,
`description`, `eheza_category`, `confidence`, `pih_mapped`,
`pih_concept_id`. The first four would force premature clinical-judgment
work that's only useful at OCL upload time; the last two would create the
"relationship to previous mapping" the user explicitly excluded.

## Walk methodology

Mechanical and deterministic — given the same source tree, the walk
produces the same rows in the same order, so re-running by hand later
yields stable IDs.

**Walk order:**
1. **Files**: 37 in-scope source files, alphabetical by repo-relative
   path. So `client/src/elm/Backend/AcuteIllnessActivity/Model.elm` first,
   `client/src/elm/Backend/WellChildEncounter/Model.elm` last.
2. **Within a file**: top-level type declarations in source order
   (top of file → bottom).
3. **Within a declaration**:
   - **Record type** `type alias Foo = { ... }`: mint `Foo` as
     `record_type`, then each field in source order as `record_field`
     with `eheza_field_path = Foo.fieldName`.
   - **Union type** `type Foo = A | B C | D ...`: mint `Foo` as
     `union_type`, then each constructor in source order as
     `union_constructor` with `eheza_field_path = Foo.A` (constructor
     argument types are implicit — not enumerated separately).
   - **Non-record type alias** `type alias FooId = EntityId Foo` or
     `type alias BarMap = Dict X Y`: mint `Foo` as `record_type` with
     `datatype = N/A`. No member rows.

**ID assignment:** sequential, starting at `EHEZA-0001`, incrementing by 1
per row. **Once the file is committed, IDs are immutable.** If a future
Elm change adds a new type, it gets the next available `EHEZA-NNNN` id
regardless of where it falls in walk order — the walk-order rule only
governs the initial inventory pass.

**External references — when a record field has a type defined outside
the 37 in-scope modules** (e.g., `Gizra.NominalDate.NominalDate`,
`EntityId X`, `Json.Decode.Value`):
- Apply the datatype derivation rule from above (`NominalDate` → `Date`,
  generic `EntityId X` → `Text`, `Value` → `N/A`)
- Do not follow the reference into the foreign module
- The field gets one row; the foreign type itself does not

**External references to in-scope-but-excluded modules** (e.g., a
clinical type referencing `Backend.Dashboard.Model.SomeType`):
- The field gets a row with `datatype = N/A`
- The excluded type does not get a row

**Edge cases**:
- **Phantom-type / zero-argument union constructors**: treat exactly like
  multi-arg constructors — one `union_constructor` row.
- **Recursive types** (`type Tree = Leaf | Node Tree Tree`): mint the
  type and all constructors; recursion in the *type signature* doesn't
  produce extra rows.
- **Functions and operators inside `Model.elm`** (rare but they exist for
  utility helpers): not concepts; skipped.
- **Type signatures without declarations** (`port`, `effect`, etc.):
  skipped.
- **Comments and module-level docstrings**: skipped.

## Build process

One-shot, hand-driven. No committed generator. The implementer is
expected to use throwaway tooling to make the walk tractable, but
nothing scratch-tooling-related gets committed.

Pragmatic workflow:
1. Write a scratch Python or Elm AST walker (`/tmp/eheza-walker.py` or
   similar) that parses each `Backend/*/Model.elm` file and emits
   preliminary rows. Scratch code is NOT committed — temporary
   accelerator only.
2. Hand-inspect the preliminary output row-by-row for:
   - **Datatype judgments the heuristic got wrong** (e.g., a `String`
     field that's actually an enum-as-string)
   - **Prettified names that read badly** (e.g., `Hiv status` vs
     `HIV status`, `Muac` vs `MUAC`)
   - **Types the walker missed** (Elm syntax edge cases)
3. Commit the hand-verified CSV.

## Validation (before commit)

| Check | Command (illustrative) | Expected |
|---|---|---|
| No duplicate `id` | `awk -F, 'NR>1 {print $1}' eheza-concepts-master.csv \| sort \| uniq -d` | empty output |
| IDs contiguous from `EHEZA-0001` | `awk -F, 'NR>1 {print $1}' \| sort -u \| wc -l` equals last-line id integer suffix | matches |
| Every `eheza_field_path` resolves to real Elm source | python helper that parses the referenced `source_module` and confirms the `TypeName` / `TypeName.fieldName` / `UnionType.Constructor` exists | all resolve |
| Every `source_module` is an in-scope file | grep against the 37-file allowlist | all in allowlist |
| `datatype` values are in the allowed set | `awk -F, 'NR>1 {print $6}' \| sort -u` | only `Numeric`, `Text`, `Coded`, `Boolean`, `Date`, `N/A` |
| Count sanity | row count vs rough expected ~1000–1500 | in range |

## Repository changes

1. **Create `docs/ocl/eheza-concepts-master.csv`** — the inventory itself.
2. **Create `docs/ocl/eheza-concepts-master.md`** — short methodology doc:
   - Purpose (canonical E-Heza concept inventory; baseline for future
     external-org mapping passes)
   - Schema (reproduces the 6-column table from this spec)
   - Walk methodology (reproduces the rules + edge cases above)
   - Inventory-pass metadata: date of walk + git SHA of the source tree
     at walk time
   - Process discipline for future Elm changes: "if you add a new
     measurement type alias, new record field, or new union constructor
     to an in-scope module, append a row to `eheza-concepts-master.csv`
     with the next available `EHEZA-NNNN` id as part of the same PR"
   - Explicit non-relationship to per-encounter `<x>-concepts.csv` files
     (separate ID spaces, separate purposes)
3. **Update `docs/ocl/README.md`** — new top-level section (after
   *Encounter types not (yet) covered*) introducing the master file and
   pointing at the methodology doc. Do NOT update the coverage table —
   the master file is not part of the per-encounter mapping structure.

No `.gitignore` changes. No code changes. No edits to existing
per-encounter CSVs.

## Branch and commits

**Branch:** `docs/eheza-concepts-master`, based on `docs/ocl-pih-mappings`
(current branch). Inherits the 4 patient-registration commits we just
pushed; when this new branch is eventually opened as a PR, the diff
against `develop` will include both the patient-registration work and the
inventory work.

**Commit shape** (5 commits, in this order, each ending with `[ci skip]`
per global preference; controller asks before each commit):

1. **Spec** (this file)
2. **Plan** (`docs/superpowers/plans/2026-04-19-eheza-concepts-master.md`)
3. **Methodology doc** (`docs/ocl/eheza-concepts-master.md`) — written
   first so reviewers of commit 4 see the rules the CSV was built against
4. **Master CSV** (`docs/ocl/eheza-concepts-master.csv`)
5. **README update** (`docs/ocl/README.md`)

## Out of scope

- Mapping the inventory to PIH, UVL, or any other org (separate future passes)
- Uploading to OCL (separate future pass)
- Editing existing per-encounter `<x>-concepts.csv` / `<x>-mappings.csv` files
- Committing a generator/regenerator script
- Restructuring the existing per-encounter pair pattern
- Migrating the 217 existing PIH-mapped concepts to the new ID space
- Updating the existing per-encounter PIH `v1.1-add-patient-registration` released version on OCL
- Coverage decisions for the 4 excluded modules (`Dashboard`,
  `ResilienceMessage`, `ResilienceSurvey`, `PatientRecord` — these stay
  excluded for this pass; reopening that decision is a separate future
  brainstorm)
