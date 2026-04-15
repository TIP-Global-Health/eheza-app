# E-Heza → OCL concept mapping

This directory holds the artefacts produced while mapping E-Heza's clinical
measurements to the Open Concept Lab (OCL) dictionaries published by Partners
In Health (`PIH/PIH`). These files are review input, not code consumed by the
app — nothing here is compiled or loaded at runtime.

## What's here

Per-encounter CSV pairs proposed for publication under `TIP/EHEZA`, each row
keyed by a stable `EH-<PREFIX>-NNN` mnemonic, with one `SAME-AS` mapping per
concept pointing at the matched `PIH/PIH` code:

| Encounter | ID prefix | Concepts | Mappings | Gaps file |
|---|---|---:|---:|---|
| Prenatal | `EH-PRE` | 65 | 65 | `prenatal-gaps.md` |
| Nutrition (individual + group sessions) | `EH-NUT` | 20 | 20 | `nutrition-gaps.md` |
| Well-Child | `EH-WC` | 12 | 12 | `remaining-modules-gaps.md` |
| Acute Illness | `EH-AI` | 24 | 24 | `remaining-modules-gaps.md` |
| NCD (non-communicable disease) | `EH-NCD` | 50 | 50 | `remaining-modules-gaps.md` |
| HIV | `EH-HIV` | 24 | 24 | `remaining-modules-gaps.md` |
| Tuberculosis | `EH-TB` | 9 | 9 | `remaining-modules-gaps.md` |
| Child Scoreboard | `EH-CS` | 2 | 2 | `childscoreboard-gaps.md` |
| **Totals** | | **206** | **206** | |

Plus shared artefacts:

| File | Purpose |
|---|---|
| `translations.jsonl` | Locale strings (en/rw/rn/so) extracted from `Translate.elm` for the published concepts — reviewer reference, not yet folded into the per-encounter CSVs |
| `csv_to_ocl_json.py` | Local converter that emits OCL bulk-import JSON-lines from the CSV pairs (no upload) |
| `remaining-modules-gaps.md` | Consolidated gaps doc for Well-Child, Acute Illness, NCD, HIV, Tuberculosis, and Family Nutrition (which has no per-encounter CSV — see below) |

### Encounter types not (yet) covered

E-Heza has nine `IndividualEncounterType` constructors plus a separate
`FamilyEncounterParticipant` flow. Coverage status:

- **Home Visit** (`HomeVisitEncounter`) — no CSV. Its measurements (feeding, hygiene, food security, caring signs) overlap heavily with the Nutrition home-visit signs already classified as out-of-scope in `nutrition-gaps.md`. Skipped pending a clinical-vs-programmatic call.
- **Family Nutrition** (`FamilyEncounterParticipant`, feature-flagged `family_nutrition`) — no CSV. MUAC and Photo measurements reuse `EH-PRE-008` and the existing photo concept; the Aheza distribution measurements are program-specific and need a reviewer call (see notes in `remaining-modules-gaps.md` and the RUTF/FBF discussion in `nutrition-gaps.md`).
- **Group flows** (Group Nutrition / Education sessions / Stock Management) — out of scope for the dictionary publishing pass; these are encounter-structural rather than clinical-fact records.
- **`InmmunizationEncounter`** — marked `@todo can be removed?` in `Backend/IndividualEncounterParticipant/Model.elm`; intentionally skipped.

## Target OCL source

- **Organization**: `TIP` (TIP Global Health, created 2026-04-06 by `adamhstewart`)
- **Source mnemonic** (proposed): `EHEZA`
- **Source type**: Dictionary
- **Custom validation schema**: OpenMRS (same as PIH — required for class/datatype compatibility)
- **Default locale**: `en`
- **Supported locales**: `en`, `rw` (Kinyarwanda), `rn` (Kirundi), `so` (Somali) — matching the four languages `client/src/elm/Translate.elm` carries. PIH's own locales (en/es/fr/ht) are independent; `SAME-AS` mappings bind concepts by code, not by shared language, so the mismatch is expected

## Scope of this pass

- **Encounter types**: 8 of the 9 `IndividualEncounterType` constructors — Prenatal, Nutrition, Well-Child, Acute Illness, NCD, HIV, Tuberculosis, Child Scoreboard. Home Visit, Family Nutrition (`FamilyEncounterParticipant`), and the deprecated `InmmunizationEncounter` are not covered by per-encounter CSVs in this pass — see *Encounter types not (yet) covered* above.
- **Match rule**: only concepts with a resolvable PIH equivalent are included in the per-encounter CSVs; everything else lands in the corresponding `*-gaps.md` (or in `remaining-modules-gaps.md` for Well-Child / Acute Illness / NCD / HIV / Tuberculosis / Family Nutrition).
- **Granularity**: one concept per measurable fact — numeric fields (vitals, anthropometry, labs), coded single-value findings, and drug concepts for medications.
- **Reuse over duplication**: when an enum constructor in a later encounter pass already corresponds to a concept published from an earlier one (e.g., `NCDASign.ChildGotDiarrhea` reusing `EH-NUT-008` Diarrhea), the existing concept's `eheza_field_path` is extended rather than minting a new `EH-` id. The gaps files document each such reuse so reviewers can trace it.
- **Deferred**: sub-record constructs (full symptom review trees, mental-health screening, breastfeeding qualitative fields, encounter-level diagnosis union) — listed at the bottom of the respective gaps files; programmatic / supply / social-determinant constructs (FBF, Ongera, NCDA program signs, water source, income source) are documented in `nutrition-gaps.md`.

## CSV schema

Both CSVs follow the column layout consumed by
[`ocldev/ocl_csv_to_json_flex`](https://github.com/OpenConceptLab/ocldev), the
de-facto tool OpenMRS/PIH implementers use to convert CSV into OCL's
JSON-lines bulk-import format.

### Concept CSV columns (`*-concepts.csv`)

| Column | Description |
|---|---|
| `resource_type` | Literal `Concept` |
| `owner` / `owner_type` | `TIP` / `Organization` |
| `source` | `EHEZA` |
| `id` | Stable mnemonic `EH-<PREFIX>-NNN` (per-encounter prefix — see *What's here*) |
| `concept_class` | OpenMRS class (Finding, Symptom, Diagnosis, Drug, Test, Question, …) |
| `datatype` | OpenMRS datatype (Numeric, Coded, Boolean, Date, N/A, …) |
| `name` | Canonical English name |
| `description` | Short clinical description |
| `eheza_category` | Informal grouping (Vitals / Anthropometry / Labs / …) — retained for reviewer context |
| `eheza_field_path` | Back-reference to the Elm type/field in `client/src/elm/Backend/Measurement/Model.elm` |
| `confidence` | `high` / `medium` / `low` — the matcher's confidence in the PIH alignment |

Non-English locale names are **not** filled in this pass. Extracting them
from `Translate.elm` requires parsing the enum-constructor → TranslationId
dispatch functions (e.g., `translateBreastExamSign`, `translateDangerSign`),
and per CLAUDE.md any non-English field that is `Nothing` falls back to
English at runtime — so auto-populating with the English string would
pollute OCL with unverified translations. Kinyarwanda / Kirundi / Somali
names should be filled in a dedicated follow-up by a native-speaker
reviewer, pulling from `Translate.elm` where the value is `Just _`.

### Mapping CSV columns (`*-mappings.csv`)

| Column | Description |
|---|---|
| `resource_type` | Literal `Mapping` |
| `owner` / `owner_type` / `source` | `TIP` / `Organization` / `EHEZA` |
| `from_concept_url` | `/orgs/TIP-Global-Health/sources/EHEZA/concepts/<EH-…-NNN>/` |
| `map_type` | `SAME-AS` (every row in this file) |
| `to_source_url` | `/orgs/PIH/sources/PIH/` |
| `to_concept_code` | PIH numeric id |
| `to_concept_name` | PIH display name — kept in the CSV for reviewer readability (OCL ignores it at import time) |
| `confidence` | Same value as the corresponding concept row |

## Methodology

The same four-step recipe was applied per encounter type:

1. **Inventory**: every measurement type alias for the encounter in `client/src/elm/Backend/Measurement/Model.elm` was enumerated, including the constituent fields of each record and the constructors of each coded enum (`DangerSign`, `BreastExamSign`, `MedicationSign`, CPE signs, NCDA signs, vaccine types, …). For Child Scoreboard, the `IndividualEncounterType` enum was also walked to confirm encounter-vs-shared-data scope.

2. **Naming**: canonical English labels were taken from `client/src/elm/Translate.elm` where an existing `TranslationId` matched the concept — either a top-level `TranslationId` constructor or a case branch inside a domain-specific translate function (`translateBreastExamSign`, `translateCorePhysicalExamSign`, `translateNCDASign`, …). No new `Translate.elm` entries were needed.

3. **PIH matching**: for each concept, the OCL search API was queried:
   ```
   https://api.openconceptlab.org/orgs/PIH/sources/PIH/concepts/?q=<term>&limit=10
   ```
   Best match was selected by name proximity → concept class fit → datatype fit → description alignment. The concept's class/datatype in the per-encounter CSV is aligned to its PIH counterpart so the SAME-AS mapping is clean.

4. **Confidence scoring**:
   - `high` — exact or near-exact name match with correct class and datatype
   - `medium` — clinically equivalent but with minor naming, class, or representation drift (e.g., set-of-dates vs. derived count for `EH-CS-002`)
   - `low` — plausible match but the reviewer should confirm (also surfaced in the encounter's gaps file)

## Verification

Run any of these to sanity-check an entry before upload.

```bash
# Resolve a single PIH concept referenced in mappings.csv
curl -s https://api.openconceptlab.org/orgs/PIH/sources/PIH/concepts/13028/ \
  | python3 -c "import sys,json; d=json.load(sys.stdin); print(d['display_name'], '|', d['concept_class'], '|', d['datatype'])"
# -> Fundal height | Finding | Numeric

# Confirm every to_concept_code in any mappings CSV resolves (spot-check loop)
awk -F, 'NR>1 {print $8}' docs/ocl/prenatal-mappings.csv | shuf -n 10 | \
  while read id; do
    echo -n "PIH:$id -> "
    curl -s "https://api.openconceptlab.org/orgs/PIH/sources/PIH/concepts/$id/" \
      | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('display_name','MISSING'))"
  done
```

Row counts (for coverage math) — every concept has exactly one SAME-AS mapping,
so concept and mapping rows must match per encounter:

```bash
for f in docs/ocl/*-concepts.csv; do
  base="${f%-concepts.csv}"
  c=$(($(wc -l < "$base-concepts.csv") - 1))
  m=$(($(wc -l < "$base-mappings.csv") - 1))
  printf "%-30s %3d concepts / %3d mappings\n" "$(basename $base)" "$c" "$m"
done
```

Optional: convert to OCL JSON-lines locally (no upload):

```bash
# Outside this repo — https://github.com/OpenConceptLab/ocldev
pip install ocldev
python -m ocldev.oclcsvtojsonconverter \
  --csv docs/ocl/prenatal-concepts.csv \
  --csv docs/ocl/prenatal-mappings.csv \
  --out prenatal.jsonl
```

## Not done in this phase

- No `TIP/EHEZA` source created on OCL (requires an OCL account with TIP org membership; deferred until CSVs are reviewed).
- No concept codes referenced from E-Heza Elm or Drupal code — the mapping is external-only for now.
- No Kinyarwanda / Kirundi / Somali name rows (see schema note above).
- No coverage for Home Visit, Family Nutrition, group-session, education-session, or stock-management flows (see *Encounter types not (yet) covered* above for the rationale).

## Where the Elm code lives

If you need to trace a concept back to the source of truth:

- `client/src/elm/Backend/Measurement/Model.elm` — every measurement type alias and coded enum across all encounter types (the `eheza_field_path` column points here)
- `client/src/elm/Backend/<Encounter>Activity/Model.elm` — per-encounter activity catalogues (e.g., `ChildScoreboardActivity`, `WellChildActivity`, `NutritionActivity`)
- `client/src/elm/Backend/Measurement/Encoder.elm` — the field keys used on the wire (match the Drupal field machine names)
- `server/hedley/modules/custom/hedley_<encounter>/hedley_<encounter>.features.field_instance.inc` — per-encounter Drupal field definitions
- `client/src/elm/Translate.elm` — canonical English (and other-locale) labels
