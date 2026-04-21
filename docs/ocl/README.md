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
| Home Visit | `EH-HV` | 1 | 1 | `homevisit-gaps.md` |
| Patient Registration | `EH-PER` | 10 | 10 | `patient-registration-gaps.md` |
| **Totals** | | **217** | **217** | |

Plus shared artefacts:

| File | Purpose |
|---|---|
| `translations.jsonl` | Locale strings (en/rw/rn/so) extracted from `Translate.elm` for the published concepts — reviewer reference, not yet folded into the per-encounter CSVs |
| `csv_to_ocl_json.py` | Local converter that emits OCL bulk-import JSON-lines from *all* CSV pairs (no upload) |
| `delta_upload.py` | Same shape, but emits only the delta against `TIP-Global-Health/EHEZA` HEAD — the workflow to use for additions / edits (see *Uploading changes* below). Requires `OCL_API_TOKEN`. |
| `remaining-modules-gaps.md` | Consolidated gaps doc for Well-Child, Acute Illness, NCD, HIV, Tuberculosis, and Family Nutrition (which has no per-encounter CSV — see below) |

### Encounter types not (yet) covered

E-Heza has nine `IndividualEncounterType` constructors plus a separate
`FamilyEncounterParticipant` flow. Coverage status:

- **Home Visit** (`HomeVisitEncounter`) — `EH-HV` (1 concept). Its measurements are reused `Nutrition*` types; most are WASH / social-determinant / programme-structural facts without PIH equivalents. See `homevisit-gaps.md` for the full coverage map.
- **Family Nutrition** (`FamilyEncounterParticipant`, feature-flagged `family_nutrition`) — no CSV. MUAC and Photo measurements reuse `EH-PRE-008` and the existing photo concept; the Aheza distribution measurements are program-specific and need a reviewer call (see notes in `remaining-modules-gaps.md` and the RUTF/FBF discussion in `nutrition-gaps.md`).
- **Group flows** (Group Nutrition / Education sessions / Stock Management) — out of scope for the dictionary publishing pass; these are encounter-structural rather than clinical-fact records.
- **`InmmunizationEncounter`** — marked `@todo can be removed?` in `Backend/IndividualEncounterParticipant/Model.elm`; intentionally skipped.

## Master inventory — `eheza-concepts-master.csv`

Separate from the per-encounter PIH-mapped pairs above, `eheza-concepts-master.csv`
is a canonical inventory of every concept E-Heza captures, drawn from a deterministic
walk of 36 of the 40 `client/src/elm/Backend/*/Model.elm` files (excluded:
`Dashboard`, `ResilienceMessage`, `ResilienceSurvey`, `PatientRecord`). Each row
gets a fresh `EHEZA-NNNN` id; the file has no relationship to the per-encounter
`<x>-concepts.csv` / `<x>-mappings.csv` files (separate ID space, separate purpose).

The master exists as the search baseline for future cross-organisation mapping
passes (UVL-Burundi is a known upcoming consumer). The per-encounter PIH-mapped
pairs continue to be the authoritative artefacts for PIH mappings; the master is
not an OCL-import-ready file (no `concept_class` / `description` columns) and is
not currently uploaded to OCL.

See `eheza-concepts-master.md` for schema, walk methodology, and the process
discipline for keeping the file in sync with future Elm changes.

## Labels inventory — `eheza-concepts-translate.csv`

A second inventory complementing `eheza-concepts-master.csv`, built from
`client/src/elm/Translate.elm` (the `type TranslationId` block, lines
329–2192 of source). Catalogues user-facing labels that survive a refined
heuristic filter — including derived clinical concepts (Gravida, BMI,
ApgarScore, etc.) that the structural master can't surface because they
exist only as UI labels, not as Elm record fields or union constructors.

Each row gets an `EHEZA-T-NNNN` id; the file has no relationship to the
structural master or to the per-encounter PIH-mapped CSVs (separate ID
spaces, separate purposes). Both masters can be consumed independently
or compared by future cross-organisation mapping passes.

Schema: 6 columns (`id`, `translation_id`, `english`, `kinyarwanda`,
`kirundi`, `somali`). Header rows for union-arg constructors carry only
the prettified english label; their value-level translations live on
leaf rows with `<UnionType>.<Value>` translation_ids.

See `eheza-concepts-translate.md` for schema, walk methodology, the
heuristic ruleset, and the process discipline for keeping the file in
sync with future Translate.elm changes.

## Target OCL source

- **Organization**: `TIP` (TIP Global Health, created 2026-04-06 by `adamhstewart`)
- **Source mnemonic** (proposed): `EHEZA`
- **Source type**: Dictionary
- **Custom validation schema**: OpenMRS (same as PIH — required for class/datatype compatibility)
- **Default locale**: `en`
- **Supported locales**: `en`, `rw` (Kinyarwanda), `rn` (Kirundi), `so` (Somali) — matching the four languages `client/src/elm/Translate.elm` carries. PIH's own locales (en/es/fr/ht) are independent; `SAME-AS` mappings bind concepts by code, not by shared language, so the mismatch is expected

## Scope of this pass

- **Encounter types**: 9 of the 9 `IndividualEncounterType` constructors — Prenatal, Nutrition, Well-Child, Acute Illness, NCD, HIV, Tuberculosis, Child Scoreboard, Home Visit. Family Nutrition (`FamilyEncounterParticipant`) and the deprecated `InmmunizationEncounter` remain without per-encounter CSVs — see *Encounter types not (yet) covered* above. Plus a Patient Registration mapping group (`EH-PER`) sourced from the `Person` record — not an encounter type, but the demographic / contact / socioeconomic field set captured at first contact and reused by every subsequent encounter.
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
| `eheza_field_path` | Back-reference to the Elm type/field in `client/src/elm/Backend/Measurement/Model.elm`. For patient-registration rows (`EH-PER-*`), this column points at the field in `client/src/elm/Backend/Person/Model.elm` instead. |
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
| `map_type` | `SAME-AS` for the great majority of rows. Use `NARROWER-THAN` when the E-Heza concept is a more specific subtype of the PIH target (e.g., `EH-PER-003` "National ID number" → `PIH:13173` "Identification number"), or `BROADER-THAN` for the inverse. PIH's own dictionary uses these hierarchical relations heavily; OCL preserves the directionality at import. |
| `to_source_url` | `/orgs/PIH/sources/PIH/` |
| `to_concept_code` | PIH numeric id |
| `to_concept_name` | PIH display name — kept in the CSV for reviewer readability (OCL ignores it at import time) |
| `confidence` | Same value as the corresponding concept row |

## Methodology

The same four-step recipe was applied per encounter type:

1. **Inventory**: every measurement type alias for the encounter in `client/src/elm/Backend/Measurement/Model.elm` was enumerated, including the constituent fields of each record and the constructors of each coded enum (`DangerSign`, `BreastExamSign`, `MedicationSign`, CPE signs, NCDA signs, vaccine types, …). For Child Scoreboard, the `IndividualEncounterType` enum was also walked to confirm encounter-vs-shared-data scope.

   *Variant for the Patient Registration pass:* inventory walks the `Person` record fields in `client/src/elm/Backend/Person/Model.elm` rather than encounter measurement aliases. Steps 2–4 below are unchanged.

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

## Uploading changes

**Always upload the delta, not the full set.** Re-submitting unchanged rows
makes OCL mint a fresh concept-version for each one, which clutters source-wide
search with duplicate snapshot rows (one per release cut). `delta_upload.py`
fetches HEAD state from OCL and emits JSON-lines only for concepts / mappings
that are new or whose content differs.

```bash
export OCL_API_TOKEN=<your-token>

# 1. compute the delta (writes JSONL to stdout, summary to stderr)
python3 docs/ocl/delta_upload.py > /tmp/eheza-delta.jsonl

# 2. upload (skip if stdout was empty)
curl -X POST "https://api.openconceptlab.org/importers/bulk-import/?update_if_exists=true" \
     -H "Authorization: Token $OCL_API_TOKEN" \
     -F "file=@/tmp/eheza-delta.jsonl"
# → returns {"id": "<task-id>", "state": "PENDING", ...}

# 3. poll task until state=SUCCESS (normally 5–25s)
curl -s -H "Authorization: Token $OCL_API_TOKEN" \
     "https://api.openconceptlab.org/importers/bulk-import/?task_id=<task-id>" \
  | python3 -m json.tool | grep -E '"(state|message|summary)"'

# 4. cut a released source version so search sees the new content
curl -X POST "https://api.openconceptlab.org/orgs/TIP-Global-Health/sources/EHEZA/versions/" \
     -H "Authorization: Token $OCL_API_TOKEN" \
     -H "Content-Type: application/json" \
     -d '{"id":"vX.Y-<short-slug>","description":"<what changed>","released":true}'
```

Step 4 is not optional — the bulk-import endpoint writes to HEAD only; search
and version-pinned URLs surface content only from released versions. Omit it
and new concepts appear invisible.

`delta_upload.py` also reports any **orphans** (concepts on OCL that are not in
the local CSVs). It deliberately does not delete them — retire or purge them
by hand if you really want them gone, so nothing accidentally disappears from
a release history.

## Not done in this phase

- No `TIP/EHEZA` source created on OCL (requires an OCL account with TIP org membership; deferred until CSVs are reviewed).
- No concept codes referenced from E-Heza Elm or Drupal code — the mapping is external-only for now.
- No Kinyarwanda / Kirundi / Somali name rows (see schema note above).
- No coverage for Home Visit, Family Nutrition, group-session, education-session, or stock-management flows (see *Encounter types not (yet) covered* above for the rationale).

## Where the Elm code lives

If you need to trace a concept back to the source of truth:

- `client/src/elm/Backend/Measurement/Model.elm` — every measurement type alias and coded enum across all encounter types (the `eheza_field_path` column points here)
- `client/src/elm/Backend/Person/Model.elm` — the `Person` record (demographic / contact / socioeconomic fields captured at registration; the `eheza_field_path` source for `EH-PER-*` concepts)
- `client/src/elm/Backend/<Encounter>Activity/Model.elm` — per-encounter activity catalogues (e.g., `ChildScoreboardActivity`, `WellChildActivity`, `NutritionActivity`)
- `client/src/elm/Backend/Measurement/Encoder.elm` — the field keys used on the wire (match the Drupal field machine names)
- `server/hedley/modules/custom/hedley_<encounter>/hedley_<encounter>.features.field_instance.inc` — per-encounter Drupal field definitions
- `client/src/elm/Translate.elm` — canonical English (and other-locale) labels
