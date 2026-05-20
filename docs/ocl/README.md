# E-Heza → Open Concept Lab dictionary

This directory holds the source-of-truth artefacts for E-Heza's clinical
concept dictionary published to Open Concept Lab (OCL) under
[`TIP-Global-Health/EHEZA`](https://app.openconceptlab.org/#/orgs/TIP-Global-Health/sources/EHEZA/).

The dictionary catalogues every concept E-Heza captures (signs, symptoms,
diagnoses, medications, lab tests, vaccines, education topics,
operational scaffolding) plus mappings into three reference dictionaries:
PIH/PIH (218 SAME-AS / NARROWER-THAN mappings), UVL-Burundi/uvl
(75 SAME-AS / NARROWER-THAN / BROADER-THAN mappings), and CIEL/CIEL
(427 SAME-AS mappings).

The dictionary is **not** loaded at runtime by the E-Heza app; it exists
to give partner organisations a structured reference for E-Heza's data
model.

## Files

| File | Purpose |
|---|---|
| `eheza-concepts.csv` | The single concept inventory (1579 rows). One row per concept; columns `id, translation_id, english, kinyarwanda, kirundi, somali, concept_class, datatype, description`. |
| `pih-mappings.csv` | 218 SAME-AS / NARROWER-THAN mappings from E-Heza concepts to PIH dictionary entries — 217 EH-XX-NNN concepts curated per-encounter, plus 1 hand-curated EHEZA-U addition. |
| `uvl-mappings.csv` | 75 SAME-AS / NARROWER-THAN / BROADER-THAN mappings to UVL-Burundi/uvl. |
| `ciel-mappings.csv` | 427 SAME-AS mappings to CIEL/CIEL — 195 derived transitively through PIH, 229 from exact-name matching, 3 hand-curated. See *CIEL mappings* below. |
| `upload.py` | OCL bulk-import uploader. Reads the four CSVs, computes the delta against OCL HEAD, emits JSON-lines payloads to stdout, prints a counts summary to stderr. |
| `labels-master-drop-tids.txt` | Hand-curation record from the v2.0 build: 369 `translation_id`s explicitly removed from `Translate.elm` walk because they were UI atoms, lab-range enums, or workflow scaffolding rather than clinical concepts. Required by any future rebuild from source. |
| `master-drop-names.txt` | Hand-curation record: 116 concept names removed from the structural walk to mirror the labels curation. Required by any future rebuild from source. |

## ID conventions

| Prefix | Meaning |
|---|---|
| `EH-AI-NNN`, `EH-CS-NNN`, `EH-HIV-NNN`, `EH-HV-NNN`, `EH-NCD-NNN`, `EH-NUT-NNN`, `EH-PER-NNN`, `EH-PRE-NNN`, `EH-TB-NNN`, `EH-WC-NNN` | Per-encounter PIH-mapped concepts curated in the v1 round. 217 total. |
| `EHEZA-U-NNNN` | Concepts added in the v2.0 unified build from the structural and labels masters. 1362 total. |

IDs are immutable once published. New concepts get the next free number
in their respective namespace.

## Concept CSV schema

`eheza-concepts.csv` columns:

| Column | Description |
|---|---|
| `id` | The OCL concept id (see ID conventions above). |
| `translation_id` | The Elm origin pointer — either a `Translate.elm` `TranslationId` constructor (e.g. `BirthDefect.DefectInguinalHernia`), or for fallback rows the Backend struct field-path (e.g. `AcuteIllnessEncounter.participant`). Used to trace a concept back to source. |
| `english` | Primary concept name (English). Sourced from `Translate.elm` where available, otherwise a CamelCase split of the Elm identifier. |
| `kinyarwanda` / `kirundi` / `somali` | Locale translations. Empty when the concept has no entry in that locale (the Elm `Translate.elm` value was `Nothing`). For EH-XX-NNN rows the translations are pulled from OCL itself, since some were curated post-upload. |
| `concept_class` | OCL OpenMRS concept class. One of: `Symptom`, `Drug`, `Diagnosis`, `Finding`, `Test`, `Procedure`, `Question`, `Misc`, `Coded clinical / socioeconomic`, `Pharmacologic Drug Class`. **Only OCL-accepted values** — invented classes (e.g. "Coded answer") are rejected. |
| `datatype` | OCL datatype. One of: `Coded`, `Numeric`, `Text`, `Boolean`, `Date`, `N/A`. Most clinical concepts are `Coded`; field-derived concepts use the type's natural datatype. |
| `description` | One-sentence dictionary-style description. Populated for all 908 clinical concepts; intentionally blank for operational/Misc rows. |

## Mapping CSV schema

All three mapping files — `pih-mappings.csv`, `uvl-mappings.csv`, and
`ciel-mappings.csv` — share the OCL bulk-import mapping schema:

| Column | Notes |
|---|---|
| `resource_type` | Always `Mapping` |
| `owner`, `owner_type`, `source` | `TIP-Global-Health` / `Organization` / `EHEZA` |
| `from_concept_url` | `/orgs/TIP-Global-Health/sources/EHEZA/concepts/<id>/` — the EHEZA side |
| `map_type` | `SAME-AS`, `NARROWER-THAN`, or `BROADER-THAN` |
| `to_source_url` | `/orgs/PIH/sources/PIH/`, `/orgs/UVL-Burundi/sources/uvl/`, or `/orgs/CIEL/sources/CIEL/` |
| `to_concept_code` | The target dictionary's concept id |
| `to_concept_name` | Display name of the target concept (denormalised for readability) |
| `confidence` | `high` (exact name match) or `medium` (token overlap / partial alignment) |

## Build pipeline (how `eheza-concepts.csv` was constructed)

The unified concept list was built from two parallel walks, then merged
and curated by hand. This section documents the pipeline so the build
can be repeated when the codebase evolves significantly enough to
warrant a new release.

### 1. Structural walk

Walks `client/src/elm/Backend/*/Model.elm` (36 files) plus
`Backend/AcuteIllnessEncounter/Types.elm`,
`Backend/NCDEncounter/Types.elm`, and
`Backend/PrenatalEncounter/Types.elm` (3 files). Excluded: `Dashboard`,
`ResilienceMessage`, `ResilienceSurvey`, `PatientRecord`.

For each `type`, `type alias`, and union constructor, emits a row with
the identifier, an Elm field-path, and a derived datatype. Drops rows
under `Msg` / `Model` types (Elm Architecture scaffolding).

For each row, looks up the bare identifier in `Translate.elm`'s
`translationSet` to pull the English + locale strings; cascading
lookup tries `<name>` then `<name>Label`. Falls back to a CamelCase
split when no `Translate.elm` match is found.

Deduplicates by `name.strip().lower()`. Drops rows whose translation
matches the phrase / question / instruction filter (sentence-leading
words like `Do`, `Is`, `Provide`, `If you`, etc.; multi-sentence
bodies; specific tid patterns like `*Question`, `*Inform`,
`ResilienceMessage*`).

### 2. Labels walk

Independent walk of every `{ english = ..., kinyarwanda = ...,
kirundi = ..., somali = ... }` record literal under
`translationSet` in `Translate.elm`. Emits one row per literal —
both top-level zero-arg branches and inner case branches one level
deep. Same phrase/question/instruction filter applied.

The walker is deliberately independent of the structural walk so it
captures concepts that exist only as UI labels (e.g.
`BirthDefect.DefectInguinalHernia`, `Gravida`, `BMI`, `ApgarScore`).

### 3. Hand curation

After the two walks produced their CSVs, a manual review removed:

- 369 `translation_id`s from the labels output (recorded in
  `labels-master-drop-tids.txt`) — generic UI atoms (`Yes`, `No`,
  `Save`, `Cancel`, …), lab-range enums (`LaboratoryPHValue.*`,
  `LabResultsNormalRange.*`, …), workflow / system messages
  (`BeginNewEncounter`, `ResultsPending`, `BackendError`, …), Z-score
  scaffolding rows.
- 116 concept names from the structural output (recorded in
  `master-drop-names.txt`) to mirror the labels curation.

Both lists are kept so a future rebuild can re-apply the curation
deterministically.

### 4. Merge

The struct-only set (concepts whose `english` is unique to the
structural walk — i.e. UI scaffolding without a `Translate.elm` entry)
was further hand-curated down to a clinically-meaningful core. The
final unified file is the labels-master content plus this curated
struct-only subset, with EH-XX-NNN ids preserved for the 217
already-uploaded concepts (matched by english).

### 5. Locale enrichment

For the 217 EH-XX-NNN rows, locale translations were re-pulled from
OCL itself (not from `Translate.elm`) because OCL's stored versions
include translations curated post-upload that don't appear in the Elm
source.

### 6. Description generation

908 clinical-class concepts (`Diagnosis`, `Drug`, `Symptom`, `Finding`,
`Test`, `Procedure`, `Coded answer` → folded into `Misc`,
`Pharmacologic Drug Class`) received hand-written one-sentence
descriptions. Operational rows (`Misc`, `Question`) intentionally left
without descriptions — OCL accepts empty.

## CIEL mappings (how `ciel-mappings.csv` was constructed)

`ciel-mappings.csv` maps E-Heza concepts to the CIEL reference
dictionary (`CIEL/CIEL` on OCL). UVL-Burundi's OpenMRS runs a
CIEL-derived dictionary, so a CIEL mapping is the bridge that lets an
E-Heza concept resolve onto a UVL concept. The file was built by a
two-stage hybrid pass; like the build pipeline above, the generator
scripts are one-offs and are not committed.

### 1. Derive through PIH

For each of the 217 EH-XX-NNN concepts already mapped in
`pih-mappings.csv`, the PIH concept is fetched from OCL and its own
`PIH → CIEL` mapping is read. The two hops are composed into one
`EHEZA → CIEL` map type:

- `SAME-AS` then `SAME-AS` → `SAME-AS`
- any `NARROWER-THAN` in the chain → `NARROWER-THAN`
- any `BROADER-THAN` in the chain → `BROADER-THAN`
- a `NARROWER-THAN` + `BROADER-THAN` contradiction → skipped

The `EHEZA → PIH` confidence (`high` / `medium`) carries through.
Stage 1 yielded **195 mappings**. 22 concepts were skipped — 20 whose
PIH concept carries no CIEL mapping, 2 whose PIH concept has multiple
SAME-AS CIEL targets — and fell through to stage 2.

### 2. Exact-match gap-fill

Every E-Heza concept not covered by stage 1 is searched against
`CIEL/CIEL`. A mapping is emitted only when **exactly one** non-retired
CIEL concept has an English name — fully-specified or synonym, compared
case-insensitively with whitespace collapsed — *exactly equal* to the
E-Heza concept's English name. Zero matches, or two or more distinct
concepts, leave the concept unmapped. Every emitted row is `SAME-AS` /
`high`.

This is deliberately conservative: exact string identity only, no fuzzy
or token matching, favouring precision over coverage. Stage 2 yielded
**229 mappings**; 1,146 concepts had no exact CIEL match and 8 were
ambiguous.

### 3. Hand-curated additions

A targeted manual pass picked up concepts whose CIEL equivalent exists
under wording the exact-match rule could not catch:

- `EHEZA-U-1345` "Birth Date" → CIEL 166575 "Date of birth"
- `EHEZA-U-1347` "Number Of Children" → CIEL 1825 "Total number of living children"
- `EHEZA-U-1349` "Spouse Name" → CIEL 161135 "Partner full name"

All three are `SAME-AS` / `medium` — the `medium` confidence flags a
curated non-exact match. The same pass confirmed CIEL has no usable
concept for the spouse / next-of-kin phone numbers or the HMIS number.

### Totals

427 CIEL mappings — 195 derived + 229 exact-match + 3 hand-curated —
all `SAME-AS` (397 `high`, 30 `medium`). The `medium` rows are the 27
derived mappings that inherit `medium` confidence from their
`EHEZA → PIH` link, plus the 3 hand-curated additions.

## OCL upload process

### Prerequisites

```bash
export OCL_API_TOKEN=<your-token>
```

The token belongs to a user with edit permission on the
`TIP-Global-Health/EHEZA` source.

### Dry-run

```bash
python3 docs/ocl/upload.py > /tmp/delta.jsonl 2> /tmp/delta.log
cat /tmp/delta.log
```

Prints a delta summary to stderr (counts of new / updated / unchanged
concepts and mappings, plus samples) and writes the import payload to
stdout. Local-only; no writes to OCL.

### Live upload

OCL's bulk-import endpoint expects multipart/form-data, not raw body:

```python
import os, urllib.request
url = "https://api.openconceptlab.org/importers/bulk-import/?update_if_exists=true&parallel=1"
body = open('/tmp/delta.jsonl', 'rb').read()
boundary = "----eheza-bulk-import"
mp = (f"--{boundary}\r\n"
      f'Content-Disposition: form-data; name="file"; filename="delta.jsonl"\r\n'
      f"Content-Type: application/octet-stream\r\n\r\n").encode() + body + f"\r\n--{boundary}--\r\n".encode()
req = urllib.request.Request(url, data=mp, headers={
    "Authorization": f"Token {os.environ['OCL_API_TOKEN']}",
    "Content-Type": f"multipart/form-data; boundary={boundary}",
})
with urllib.request.urlopen(req) as r:
    print(r.read().decode())  # returns task_id
```

Use `parallel=1` on first runs of a large delta to avoid intra-batch
race conditions on locale-name uniqueness; `parallel=4` is safe for
small delta runs.

### Polling

The bulk-import detail endpoint with task id returns `[]` (a known OCL
quirk). Use the list endpoint and grep for the task:

```python
url = "https://api.openconceptlab.org/importers/bulk-import/"
# paginate, find task by id, watch state until SUCCESS / FAILURE
```

Per-batch failure counts live in the child task summaries
(`/tasks/<child_id>/`); the parent task message **omits failure counts**.
After a job completes, always cross-check by re-fetching the source
concept list and diffing against `eheza-concepts.csv` to verify nothing
was silently dropped.

### Cutting a released source version

After the concept and mapping uploads complete cleanly, cut a released
version so the new concepts become visible to dictionary search:

```python
import json, urllib.request
url = "https://api.openconceptlab.org/orgs/TIP-Global-Health/sources/EHEZA/versions/"
payload = json.dumps({"id": "v2.1", "description": "...", "released": True}).encode()
req = urllib.request.Request(url, data=payload, headers={
    "Authorization": f"Token {os.environ['OCL_API_TOKEN']}",
    "Content-Type": "application/json",
})
with urllib.request.urlopen(req) as r:
    print(r.status, r.read().decode())
```

Cutting a released version is **mandatory** — without it, newly
imported concepts stay invisible to OCL dictionary search even though
they exist on HEAD.

## Known OCL constraints (handled by `upload.py`)

1. **`(locale, FULLY_SPECIFIED-name)` uniqueness per source.** Different
   English concepts often share the same Kinyarwanda / Kirundi /
   Somali word in `Translate.elm`. The uploader tracks
   `(locale, name.lower()) → concept_id` claims and demotes colliding
   locale names from `FULLY_SPECIFIED` to `SHORT`. Comparison is
   case-insensitive (OCL's uniqueness check ignores case).
2. **Strictly-typed `concept_class`.** OCL accepts a fixed set of
   OpenMRS classes. Invented values (e.g. "Coded answer") are
   rejected. Keep `eheza-concepts.csv`'s `concept_class` to known-good
   values from the list above.
3. **Bulk-import doesn't surface per-row errors.** The job summary
   reports created/updated counts but omits failures. Always verify by
   diffing the local CSV against the post-import OCL state.

## Release history

| Version | Date | Changes |
|---|---|---|
| `v1.0` (per-encounter releases) | 2026-04-15 to 2026-04-19 | Initial 217 EH-XX-NNN concepts uploaded across 10 per-encounter rounds, each with PIH SAME-AS / NARROWER-THAN mappings. |
| `v2.0` | 2026-04-21 | Unified release. 1362 new EHEZA-U-NNNN concepts from the combined structural + labels walk. 75 UVL mappings added. Locale translations refreshed across all 217 prior concepts. Final source counts: 1579 concepts, 292 mappings (217 PIH + 75 UVL). |

## Future expansion

Adding a new concept:

1. Append a row to `eheza-concepts.csv` with the next free
   `EHEZA-U-NNNN` id (or for a per-encounter PIH-mapped concept, the
   next free id in the appropriate `EH-XX-` prefix).
2. If it has a PIH, UVL, or CIEL mapping, append a row to the relevant
   mapping file.
3. Run the dry-run to confirm the delta is just your new row.
4. Run the live upload.
5. Cut a new released source version (e.g. `v2.1`) once the upload
   completes.

Rebuilding the unified file from a fresh codebase walk:

1. Re-walk `client/src/elm/Backend/*/Model.elm` (36 files) +
   `*/Types.elm` (3 files) per the *Structural walk* steps above.
2. Re-walk `Translate.elm`'s `translationSet` per the *Labels walk*
   steps.
3. Apply the phrase/question/instruction filter (heuristic rules
   documented in the v2.0 commit history).
4. Apply `labels-master-drop-tids.txt` and `master-drop-names.txt`
   to remove hand-curated drops.
5. Merge into the unified shape, preserving EH-XX-NNN ids for
   already-published concepts (matched by english).
6. Run `upload.py` for the delta and a new released version.
