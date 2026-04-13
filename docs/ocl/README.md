# E-Heza → OCL concept mapping

This directory holds the artefacts produced while mapping E-Heza's clinical
measurements to the Open Concept Lab (OCL) dictionaries published by Partners
In Health (`PIH/PIH`). These files are review input, not code consumed by the
app — nothing here is compiled or loaded at runtime.

## What's here

| File | Purpose |
|---|---|
| `prenatal-concepts.csv` | 70 Prenatal concepts proposed for publication under `TIP/EHEZA`, each row keyed by a stable `EH-PRE-NNN` mnemonic |
| `prenatal-mappings.csv` | One `SAME-AS` mapping per concept, pointing to the matched `PIH/PIH` code |
| `prenatal-gaps.md` | Prenatal concepts that did not resolve to a PIH equivalent, plus low-confidence matches flagged for reviewer attention |

## Target OCL source

- **Organization**: `TIP` (TIP Global Health, created 2026-04-06 by `adamhstewart`)
- **Source mnemonic** (proposed): `EHEZA`
- **Source type**: Dictionary
- **Custom validation schema**: OpenMRS (same as PIH — required for class/datatype compatibility)
- **Default locale**: `en`
- **Supported locales**: `en`, `rw` (Kinyarwanda), `rn` (Kirundi), `so` (Somali) — matching the four languages `client/src/elm/Translate.elm` carries. PIH's own locales (en/es/fr/ht) are independent; `SAME-AS` mappings bind concepts by code, not by shared language, so the mismatch is expected

## Scope of this pass

- **Encounter type**: Prenatal only (pilot)
- **Match rule**: only concepts with a resolvable PIH equivalent are included in the two CSVs; everything else is in `prenatal-gaps.md`
- **Granularity**: one concept per measurable fact — numeric fields (vitals, anthropometry, labs), coded single-value findings, and drug concepts for prenatal supplements
- **Deferred**: sub-record constructs (full symptom review trees, mental-health screening, breastfeeding qualitative fields, encounter-level diagnosis union) — listed at the bottom of `prenatal-gaps.md`

## CSV schema

Both CSVs follow the column layout consumed by
[`ocldev/ocl_csv_to_json_flex`](https://github.com/OpenConceptLab/ocldev), the
de-facto tool OpenMRS/PIH implementers use to convert CSV into OCL's
JSON-lines bulk-import format.

### `prenatal-concepts.csv`

| Column | Description |
|---|---|
| `resource_type` | Literal `Concept` |
| `owner` / `owner_type` | `TIP` / `Organization` |
| `source` | `EHEZA` |
| `id` | Stable mnemonic `EH-PRE-001`…`EH-PRE-070` |
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

### `prenatal-mappings.csv`

| Column | Description |
|---|---|
| `resource_type` | Literal `Mapping` |
| `owner` / `owner_type` / `source` | `TIP` / `Organization` / `EHEZA` |
| `from_concept_url` | `/orgs/TIP/sources/EHEZA/concepts/<EH-PRE-NNN>/` |
| `map_type` | `SAME-AS` (every row in this file) |
| `to_source_url` | `/orgs/PIH/sources/PIH/` |
| `to_concept_code` | PIH numeric id |
| `to_concept_name` | PIH display name — kept in the CSV for reviewer readability (OCL ignores it at import time) |
| `confidence` | Same value as the corresponding concept row |

## Methodology

1. **Inventory**: every Prenatal measurement in `client/src/elm/Backend/Measurement/Model.elm` was enumerated, including the constituent fields of each record and the constructors of each coded enum (`DangerSign`, `BreastExamSign`, `MedicationSign`, CPE signs, …).

2. **Naming**: canonical English labels were taken from `client/src/elm/Translate.elm` where an existing `TranslationId` matched the concept. A survey of the pilot set confirmed all ~70 concepts have existing UI translations (either as top-level `TranslationId` constructors or as case branches inside domain-specific translate functions such as `translateBreastExamSign` and `translateCorePhysicalExamSign`) — no new `Translate.elm` entries were needed for this batch.

3. **PIH matching**: for each concept, the OCL search API was queried:
   ```
   https://api.openconceptlab.org/orgs/PIH/sources/PIH/concepts/?q=<term>&limit=10
   ```
   Best match was selected by name proximity → concept class fit → datatype fit → description alignment. The concept's class/datatype in `prenatal-concepts.csv` is aligned to its PIH counterpart so the SAME-AS mapping is clean.

4. **Confidence scoring**:
   - `high` — exact or near-exact name match with correct class and datatype
   - `medium` — clinically equivalent but with minor naming or class drift
   - `low` — plausible match but the reviewer should confirm (flagged also in `prenatal-gaps.md`)

## Verification

Run any of these to sanity-check an entry before upload.

```bash
# Resolve a single PIH concept referenced in mappings.csv
curl -s https://api.openconceptlab.org/orgs/PIH/sources/PIH/concepts/13028/ \
  | python3 -c "import sys,json; d=json.load(sys.stdin); print(d['display_name'], '|', d['concept_class'], '|', d['datatype'])"
# -> Fundal height | Finding | Numeric

# Confirm every to_concept_code in the mappings CSV resolves (spot-check loop)
awk -F, 'NR>1 {print $8}' docs/ocl/prenatal-mappings.csv | shuf -n 10 | \
  while read id; do
    echo -n "PIH:$id -> "
    curl -s "https://api.openconceptlab.org/orgs/PIH/sources/PIH/concepts/$id/" \
      | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('display_name','MISSING'))"
  done
```

Row counts (for coverage math):

```bash
wc -l docs/ocl/prenatal-concepts.csv docs/ocl/prenatal-mappings.csv
# Expect matching row counts (71 each, incl. header): every concept has exactly one SAME-AS mapping.
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
- No coverage beyond Prenatal. Other encounter types (Well-Child, Nutrition, NCD, Acute Illness, HIV, TB, Family Nutrition) follow the same methodology when approved.

## Where the Elm code lives

If you need to trace a concept back to the source of truth:

- `client/src/elm/Backend/Measurement/Model.elm` — every Prenatal measurement type alias and coded enum (the `eheza_field_path` column points here)
- `client/src/elm/Backend/Measurement/Encoder.elm` — the field keys used on the wire (match the Drupal field machine names)
- `server/hedley/modules/custom/hedley_prenatal/hedley_prenatal.features.field_instance.inc` — Drupal field definitions
- `client/src/elm/Translate.elm` — canonical English (and other-locale) labels
