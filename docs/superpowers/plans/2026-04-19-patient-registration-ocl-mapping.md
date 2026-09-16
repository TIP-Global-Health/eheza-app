# Patient Registration → OCL/PIH Concept Mapping — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a Patient Registration mapping group (`EH-PER` prefix) under `docs/ocl/`, parallel to the existing nine per-encounter groups, covering 27 candidate fields from the `Person` record.

**Architecture:** Pure documentation deliverable — three new files (`patient-registration-concepts.csv`, `patient-registration-mappings.csv`, `patient-registration-gaps.md`), an append to `translations.jsonl`, and edits to `README.md`. No code or schema changes. Existing `csv_to_ocl_json.py` and `delta_upload.py` glob `*-concepts.csv` / `*-mappings.csv` and pick up the new pair automatically. Upload to OCL is **out of scope**.

**Tech Stack:** CSV (de-facto `ocldev/ocl_csv_to_json_flex` schema), JSON-Lines for translations, Markdown for README and gaps doc, ad-hoc Python (`csv_to_ocl_json.py`) and `curl` for local validation. PIH/OCL probes via `https://api.openconceptlab.org/orgs/PIH/sources/PIH/concepts/?q=<term>&limit=10` (no auth required for read).

**Spec:** `docs/superpowers/specs/2026-04-19-patient-registration-ocl-mapping-design.md`

**Conventions enforced throughout:**
- Per global preference: **always ask the user before running `git commit` or `git push`** — every commit step below is "ask, then commit if approved".
- Per CLAUDE.md: every commit message ends with `[ci skip]` (after the Co-Authored-By line).
- Per README §Reuse over duplication: before minting a new `EH-PER-NNN`, grep existing `*-mappings.csv` for the same `to_concept_code`; hits move to the gaps file's *Already represented* section instead.
- Per README §Concept CSV columns + spec §Translations: never auto-fill non-English locale names with the English string. Only `Just _` values from `Translate.elm` get extracted.

---

## File Structure

**Create:**
- `docs/ocl/patient-registration-concepts.csv` — Concept rows; one per minted `EH-PER-NNN` that survives the PIH probe.
- `docs/ocl/patient-registration-mappings.csv` — One `SAME-AS` mapping row per concept row.
- `docs/ocl/patient-registration-gaps.md` — Coverage map; same shape as `homevisit-gaps.md` / `nutrition-gaps.md`.

**Modify:**
- `docs/ocl/README.md` — Coverage table row, totals, scope sentence, methodology variant, `eheza_field_path` extension note, "Where the Elm code lives" bullet.
- `docs/ocl/translations.jsonl` — Append entries keyed by new `EH-PER-NNN` ids (only fields with `Just _` rw/rn/so values).
- `docs/superpowers/specs/2026-04-19-patient-registration-ocl-mapping-design.md` — May need to commit this in step 0 if it's still uncommitted from the brainstorming pass.

**No code changes.** `csv_to_ocl_json.py` and `delta_upload.py` are not edited.

**Working / scratch files (uncommitted):**
- `/tmp/eheza-per-probes.tsv` — probe results table (TSV: field, search-term, top-hit-id, top-hit-name, top-hit-class, top-hit-datatype, confidence-judgement, reuse-id-if-any).
- `/tmp/eheza-per-translations.txt` — extracted `Translate.elm` rw/rn/so values before reformatting into JSON-Lines.

---

## Task 0: Commit the spec (if uncommitted)

**Files:**
- Stage: `docs/superpowers/specs/2026-04-19-patient-registration-ocl-mapping-design.md`

- [ ] **Step 1: Check spec commit status**

Run:
```bash
git status -- docs/superpowers/specs/2026-04-19-patient-registration-ocl-mapping-design.md
```

Expected: either "untracked" / "modified" (needs commit) or "nothing to commit" (skip rest of this task).

- [ ] **Step 2: Ask user before committing**

If uncommitted, tell the user: *"Spec file is uncommitted. May I commit it?"* and wait for explicit yes.

- [ ] **Step 3: Commit the spec**

```bash
git add docs/superpowers/specs/2026-04-19-patient-registration-ocl-mapping-design.md
git commit -m "$(cat <<'EOF'
Add patient-registration OCL mapping spec

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

Expected: commit succeeds; pre-commit hooks (if any) pass.

---

## Task 1: PIH probe — capture matches and confidence per candidate

This task produces the probe table that drives every later task. No artefact files yet; output is `/tmp/eheza-per-probes.tsv` (uncommitted).

**Files:**
- Write: `/tmp/eheza-per-probes.tsv` (scratch, not committed)

- [ ] **Step 1: Probe each of the 27 candidates against PIH**

For each `(field, search-term)` pair from spec §Per-concept row plan, run:

```bash
curl -s "https://api.openconceptlab.org/orgs/PIH/sources/PIH/concepts/?q=<search-term>&limit=10" \
  | python3 -c "import sys,json
for c in json.load(sys.stdin):
    print(c['id'], '|', c['display_name'], '|', c['concept_class'], '|', c['datatype'])"
```

Run a probe for each search-term listed in the spec table for each field (some fields list multiple terms — try them in order until you get a credible hit).

Record the best hit (or "no hit") in `/tmp/eheza-per-probes.tsv`, columns:

```
field<TAB>search_term_used<TAB>pih_id<TAB>pih_name<TAB>pih_class<TAB>pih_datatype<TAB>confidence<TAB>notes
```

Confidence rubric (per spec):
- `high` — exact name match + correct class + correct datatype
- `medium` — clinically equivalent but minor drift (naming, class, representation)
- `low` — plausible match, reviewer should confirm
- `none` — no PIH equivalent found (this row will land in gaps, not CSV)

- [ ] **Step 2: Sanity-check that each `pih_id` resolves**

For each row with a non-empty `pih_id`, run:

```bash
curl -s "https://api.openconceptlab.org/orgs/PIH/sources/PIH/concepts/<id>/" \
  | python3 -c "import sys,json; d=json.load(sys.stdin); print(d['display_name'], '|', d['concept_class'], '|', d['datatype'])"
```

Expected: a populated response (no 404, no MISSING). Update the probes file if anything's wrong.

- [ ] **Step 3: Print summary**

```bash
awk -F'\t' 'NR>1 {c[$7]++} END {for (k in c) print k, c[k]}' /tmp/eheza-per-probes.tsv
```

Expected: counts of `high` / `medium` / `low` / `none`. Sanity-check the total equals 27.

No commit (scratch file only).

---

## Task 2: Reuse check — collapse matches that already exist in the CSVs

**Files:**
- Read: `docs/ocl/*-mappings.csv`
- Modify: `/tmp/eheza-per-probes.tsv` (annotate reuse column)

- [ ] **Step 1: For each non-`none` probe, check whether the `pih_id` already exists in any CSV mapping**

```bash
awk -F'\t' 'NR>1 && $3!="" {print $3}' /tmp/eheza-per-probes.tsv | sort -u | while read pid; do
  existing=$(awk -F, -v pid="$pid" '$8==pid {print FILENAME ":" $4}' docs/ocl/*-mappings.csv | head -1)
  if [ -n "$existing" ]; then
    echo "REUSE: PIH:$pid -> $existing"
  fi
done
```

Expected: zero or more `REUSE:` lines. Each one means the corresponding Person field collapses onto an existing `EH-` id and must NOT mint a new `EH-PER` row.

- [ ] **Step 2: Annotate the probes file**

For each REUSE line, add the existing `EH-` id to the `notes` column of the matching probe row (e.g., `REUSE EH-PRE-019`). These rows will land in the gaps file's *Already represented* section, not in the CSV.

- [ ] **Step 3: Verify counts match expectations**

```bash
awk -F'\t' 'NR>1 {if ($7=="none" || $8 ~ /REUSE/) gaps++; else csv++} END {print "csv=" csv " gaps_or_reuse=" gaps}' /tmp/eheza-per-probes.tsv
```

Expected: `csv` count is the number of rows that will go into `patient-registration-concepts.csv`; the rest go in the gaps file. Sanity-check the sum equals 27.

No commit (scratch file only).

---

## Task 3: Extract translations from `Translate.elm`

**Files:**
- Read: `client/src/elm/Translate.elm`
- Write: `/tmp/eheza-per-translations.txt` (scratch, not committed)

- [ ] **Step 1: For each candidate field, locate its TranslationId in `Translate.elm`**

Use Grep to find each TranslationId from the spec §Translations list:

```
FirstName, SecondName, NationalIdNumber, ChildHmisNumber, Photo, BirthDate,
Gender, HIVStatusLabel, NumberOfChildrenUnder5, ModeOfDeliveryLabel, Ubudehe,
LevelOfEducationLabel, MaritalStatusLabel, Province, District, Sector, Cell,
Village, Latitude, Longitude, TelephoneNumber
```

Plus any others surfaced during the probe pass (e.g., spouse / next-of-kin TranslationIds).

For each TranslationId, locate its translation record in `Translate.elm` and capture the `english`, `kinyarwanda`, `kirundi`, `somali` field values.

- [ ] **Step 2: Write the extraction to scratch file**

For each TranslationId found, append a line to `/tmp/eheza-per-translations.txt` in the form:

```
TranslationId<TAB>en=...<TAB>rw=Just|Nothing<TAB>rn=Just|Nothing<TAB>so=Just|Nothing
```

with the actual `Just <value>` / `Nothing` literals from the source.

- [ ] **Step 3: Verify**

Manually scan the file: every row should have at least the `en` value; rw/rn/so may be `Nothing` (those become absent fields in the JSONL row, per `csv_to_ocl_json.py`'s `value:` check at line 28).

No commit (scratch file only).

---

## Task 4: Write `patient-registration-concepts.csv`

**Files:**
- Create: `docs/ocl/patient-registration-concepts.csv`

- [ ] **Step 1: Look at an existing concepts CSV to copy the column header exactly**

Run:
```bash
head -1 docs/ocl/prenatal-concepts.csv
```

Expected output (this is the canonical header):
```
resource_type,owner,owner_type,source,id,concept_class,datatype,name,description,eheza_category,eheza_field_path,confidence
```

- [ ] **Step 2: Create the file with the header**

Write `docs/ocl/patient-registration-concepts.csv` containing exactly the header line above and no rows yet.

- [ ] **Step 3: Append one row per probe with status `high`/`medium`/`low` AND no REUSE annotation**

For each qualifying row in `/tmp/eheza-per-probes.tsv`, append a CSV row:

- `resource_type` = `Concept`
- `owner` = `TIP-Global-Health`
- `owner_type` = `Organization`
- `source` = `EHEZA`
- `id` = next sequential `EH-PER-NNN` (start at `EH-PER-001`, increment by 1, no gaps; assign in the order from spec §Per-concept row plan, skipping rows that fell to gaps/reuse so the IDs in the CSV stay contiguous starting at 001)
- `concept_class` = the matched PIH concept's class (per Task 1 step 2 lookup)
- `datatype` = the matched PIH concept's datatype
- `name` = the proposed name from the spec table (use the exact English `name` from `Translate.elm` if available; otherwise the spec's proposed name)
- `description` = one short sentence (e.g., "Patient's first / given name", "Date of birth", "Highest level of education completed"). Keep under 120 chars.
- `eheza_category` = informal grouping per spec table (`Names`, `Identifiers`, `Demographics`, `Coded clinical / socioeconomic`, `Administrative geography`, `GPS`, `Contact`, `Relationships`)
- `eheza_field_path` = `Person.<fieldName>` (e.g., `Person.firstName`, `Person.birthDate`)
- `confidence` = the probe's confidence value

CSV-quote any field that contains a comma, a double-quote, or a newline (RFC 4180; `csv` Python module handles this if you generate it programmatically).

- [ ] **Step 4: Verify the file parses as valid CSV**

Run:
```bash
python3 -c "
import csv
with open('docs/ocl/patient-registration-concepts.csv') as f:
    rows = list(csv.DictReader(f))
print(f'rows={len(rows)}, ids={[r[\"id\"] for r in rows]}')
"
```

Expected: row count matches step 3's qualifying rows; ids are `EH-PER-001` through `EH-PER-NNN` with no gaps; no parse errors.

- [ ] **Step 5: Verify every `eheza_field_path` references a real Person field**

For each `eheza_field_path` value `Person.<fieldName>`, confirm `<fieldName>` exists in `client/src/elm/Backend/Person/Model.elm`:

```bash
python3 -c "
import csv, re, pathlib
model = pathlib.Path('client/src/elm/Backend/Person/Model.elm').read_text()
fields = set(re.findall(r'\s*,?\s*(\w+)\s*:', model.split('type alias Person')[1].split('}')[0]))
with open('docs/ocl/patient-registration-concepts.csv') as f:
    for r in csv.DictReader(f):
        path = r['eheza_field_path']
        if not path.startswith('Person.') or path.split('.', 1)[1] not in fields:
            print('BAD:', r['id'], path)
print('done')
"
```

Expected: only `done` printed. Any `BAD:` line is a typo to fix before commit.

No commit yet — coupled with mappings.csv in Task 5's commit.

---

## Task 5: Write `patient-registration-mappings.csv`

**Files:**
- Create: `docs/ocl/patient-registration-mappings.csv`

- [ ] **Step 1: Look at an existing mappings CSV to copy the column header exactly**

Run:
```bash
head -1 docs/ocl/prenatal-mappings.csv
```

Expected output:
```
resource_type,owner,owner_type,source,from_concept_url,map_type,to_source_url,to_concept_code,to_concept_name,confidence
```

- [ ] **Step 2: Create the file with the header**

Write `docs/ocl/patient-registration-mappings.csv` with exactly the header line above and no rows yet.

- [ ] **Step 3: Append one mapping row per concept row**

For every row in `patient-registration-concepts.csv`, append:

- `resource_type` = `Mapping`
- `owner` = `TIP-Global-Health`
- `owner_type` = `Organization`
- `source` = `EHEZA`
- `from_concept_url` = `/orgs/TIP-Global-Health/sources/EHEZA/concepts/<EH-PER-NNN>/` (note the leading and trailing slash — the existing CSVs all do this)
- `map_type` = `SAME-AS`
- `to_source_url` = `/orgs/PIH/sources/PIH/`
- `to_concept_code` = the PIH numeric id from the probe
- `to_concept_name` = the PIH `display_name` from the probe (kept for reviewer readability — OCL ignores it at import time per README)
- `confidence` = same value as the corresponding concept row

- [ ] **Step 4: Verify row-count parity with concepts**

Run the README §Verification snippet, scoped to this pair:

```bash
for f in docs/ocl/patient-registration-concepts.csv; do
  base="${f%-concepts.csv}"
  c=$(($(wc -l < "$base-concepts.csv") - 1))
  m=$(($(wc -l < "$base-mappings.csv") - 1))
  printf "%-30s %3d concepts / %3d mappings\n" "$(basename $base)" "$c" "$m"
done
```

Expected: concepts == mappings (every concept has exactly one SAME-AS).

- [ ] **Step 5: Spot-check 5 PIH codes resolve**

Run:
```bash
awk -F, 'NR>1 {print $8}' docs/ocl/patient-registration-mappings.csv | shuf -n 5 | \
while read id; do
  echo -n "PIH:$id -> "
  curl -s "https://api.openconceptlab.org/orgs/PIH/sources/PIH/concepts/$id/" \
    | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('display_name','MISSING'))"
done
```

Expected: 5 lines, none containing `MISSING`. Any `MISSING` means a stale/typo'd id — fix before commit.

- [ ] **Step 6: Run `csv_to_ocl_json.py` to confirm the new pair converts cleanly**

```bash
python3 docs/ocl/csv_to_ocl_json.py | python3 -c "
import sys, json
n = 0
for line in sys.stdin:
    json.loads(line)
    n += 1
print(f'parsed {n} json-lines, all valid')
"
```

Expected: prints `parsed N json-lines, all valid` where N is the existing total + new patient-registration rows. Any JSON error means a CSV-quoting bug — fix and re-run.

- [ ] **Step 7: Ask user before committing**

Tell the user: *"CSVs and validation pass. May I commit them?"* and wait for explicit yes.

- [ ] **Step 8: Commit the CSV pair**

```bash
git add docs/ocl/patient-registration-concepts.csv docs/ocl/patient-registration-mappings.csv
git commit -m "$(cat <<'EOF'
Add patient-registration OCL concepts and SAME-AS mappings

Maps Person record fields (firstName, gender, marital status, ...) to PIH
dictionary concepts. New EH-PER-NNN ids; csv_to_ocl_json.py picks them up
automatically via its *-concepts.csv / *-mappings.csv glob.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

Expected: commit succeeds.

---

## Task 6: Append entries to `translations.jsonl`

**Files:**
- Modify: `docs/ocl/translations.jsonl`

- [ ] **Step 1: Look at the existing JSONL structure to copy the format exactly**

Run:
```bash
head -1 docs/ocl/translations.jsonl
```

Expected output (canonical row shape):
```
{"id": "EH-PRE-001", "kinyarwanda": "...", "kirundi": "...", "somali": null}
```

Each row is one JSON object per line. `kinyarwanda`/`kirundi`/`somali` carry either a string or `null`. Per `csv_to_ocl_json.py:28-35`, `null` (i.e. falsy) values are skipped at OCL emit time — so it's safe to write `null` for `Nothing` translations.

- [ ] **Step 2: Build new entries**

For each row in `patient-registration-concepts.csv`, look up the matching TranslationId in `/tmp/eheza-per-translations.txt` (Task 3) and emit a JSON-Lines row keyed by the `EH-PER-NNN` id:

- `id` = the concept id
- `kinyarwanda` = the `Just <value>` from `Translate.elm` if present, else `null`
- `kirundi` = same rule
- `somali` = same rule

If a concept's TranslationId has all three locales as `Nothing`, **skip that row entirely** — no point in writing an all-null entry.

- [ ] **Step 3: Append the new rows to `translations.jsonl`**

Append directly; do not re-write the file. Each new row goes on its own line at the end.

- [ ] **Step 4: Verify the file parses**

```bash
python3 -c "
import json
with open('docs/ocl/translations.jsonl') as f:
    rows = [json.loads(line) for line in f if line.strip()]
print(f'rows={len(rows)}, eh_per_rows={sum(1 for r in rows if r[\"id\"].startswith(\"EH-PER-\"))}')
"
```

Expected: total row count increased by exactly the number of new rows; `eh_per_rows` matches expectation.

- [ ] **Step 5: Re-run the converter to confirm no regression**

```bash
python3 docs/ocl/csv_to_ocl_json.py | python3 -c "
import sys, json
n = 0
for line in sys.stdin:
    json.loads(line)
    n += 1
print(f'parsed {n} json-lines, all valid')
"
```

Expected: still `all valid`; row count unchanged from Task 5 step 6 (JSONL only adds locale names to existing concept rows, doesn't create new ones).

- [ ] **Step 6: Ask user before committing**

Tell the user: *"translations.jsonl updated and validated. May I commit?"* and wait for explicit yes.

- [ ] **Step 7: Commit**

```bash
git add docs/ocl/translations.jsonl
git commit -m "$(cat <<'EOF'
Add patient-registration translations to OCL JSONL

Pulls Just-typed rw/rn/so values from Translate.elm for the new EH-PER-NNN
concepts. Nothing values become JSON null and are skipped at OCL emit time
per csv_to_ocl_json.py's locale handling.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

Expected: commit succeeds.

---

## Task 7: Write `patient-registration-gaps.md`

**Files:**
- Create: `docs/ocl/patient-registration-gaps.md`

- [ ] **Step 1: Look at the canonical gaps file shape**

Read `docs/ocl/homevisit-gaps.md` end-to-end. Its sections are the template:

1. Header paragraph
2. § New in this pass — `<encounter>-concepts.csv`
3. § Already represented in `TIP/EHEZA` — not re-created
4. § Low-confidence candidates — deferred for reviewer attention
5. § Confirmed gaps — no PIH equivalent found (YYYY-MM-DD probe)
6. § Structural items (not clinical concepts) — omitted if empty

- [ ] **Step 2: Write the file**

Create `docs/ocl/patient-registration-gaps.md` with the following sections. Replace `<...>` placeholders with values from `/tmp/eheza-per-probes.tsv`. Omit sections that end up empty.

```markdown
# Patient Registration concepts — coverage map

Patient registration (`Backend/Person/Model.elm`) is not an encounter type
but a person-level field set: the demographic, contact, and socioeconomic
data a nurse fills at first contact and that feeds every subsequent
encounter. This file documents which of the 27 candidate `Person` fields
landed in `patient-registration-concepts.csv`, which collapsed onto an
existing `TIP/EHEZA` concept, and which had no PIH equivalent.

## New in this pass — `patient-registration-concepts.csv`

| EH id | Concept | E-Heza field | PIH | Confidence |
|---|---|---|---|---|
| `EH-PER-001` | <name> | `Person.<field>` | `PIH:<id>` (`<class>/<datatype>`, *"<pih_name>"*) | <high/medium/low> |
| ... one row per CSV concept ... |

## Already represented in `TIP/EHEZA` — not re-created

| Person field | Reuses concept | Notes |
|---|---|---|
| `Person.<field>` | `EH-<XXX>-NNN` <name> | <one-line rationale> |

(Omit this section if no Person field collapses onto an existing concept.)

## Low-confidence candidates — deferred for reviewer attention

| E-Heza field | Candidate PIH match | Why excluded |
|---|---|---|
| `Person.<field>` | `PIH:<id>` *<name>* (`<class>/<datatype>`) | <one-line drift description> |

(Omit this section if there are no low-confidence rows.)

## Confirmed gaps — no PIH equivalent found (<YYYY-MM-DD> probe)

Searches run on `https://api.openconceptlab.org/orgs/PIH/sources/PIH/concepts/?q=<term>`
— logging the negative results so the next mapping pass doesn't re-search
the same terms.

### Personal identifiers — modeled in OCL as person attributes, not concepts

- **`Person.firstName` / `Person.secondName`** — searches: `first+name`, `family+name`, `surname`, `given+name`. PIH's hits are person-name *attribute* records, not standalone concepts in the dictionary; mapping a SAME-AS would conflate person-attribute storage with clinical-concept reuse.
- **`Person.nationalIdNumber`** — search `national+id` / `national+identification`. Similar rationale.
- **`Person.hmisNumber`** — programme-specific identifier; PIH has no generic HMIS-number concept.
- **`Person.avatarUrl`** — patient photo URL; not a clinical concept.
- **`Person.telephoneNumber`** / **`Person.spousePhoneNumber`** / **`Person.nextOfKinPhoneNumber`** / **`Person.spouseName`** / **`Person.nextOfKinName`** — contact details; person attributes, not concepts.

(Only include the bullet for a field if Task 1's probe actually returned `none`.
If the probe did find a match, the field belongs in the *New in this pass* table
above instead.)

### Administrative geography — no PIH dictionary coverage

- **`Person.province` / `district` / `sector` / `cell` / `village`** — Rwanda/Burundi admin division strings. Searches return location-of-care concepts (`PIH:8395`-style) that aren't admin-division equivalents. Modeled site-side as free-text fields.

### Rwanda-specific

- **`Person.ubudehe`** (`Ubudehe1`..`Ubudehe4`) — Rwandan socioeconomic stratification. Search `ubudehe` / `socioeconomic+category` returns no equivalent.

### Operational toggles

- **`Person.saveGPSLocation`** — UI consent flag for capturing GPS, not a clinical fact.
- **`Person.registrationLatitude` / `registrationLongitude`** — captured only if the consent flag is set; PIH has no generic patient-coordinates concept (lat/lon are typically location attributes, not concepts).

(Only include the bullet for a field if Task 1's probe returned `none`.)

### Metadata flags

- **`Person.isDateOfBirthEstimated`** — modifies the interpretation of `birthDate` rather than being a standalone clinical fact.

(Only include the bullet for a field if Task 1's probe returned `none`.)
```

- [ ] **Step 3: Verify the file renders**

Run:
```bash
python3 -c "
import re, pathlib
text = pathlib.Path('docs/ocl/patient-registration-gaps.md').read_text()
print('headings:', re.findall(r'^##+ .*', text, re.M))
print('eh_ids:', re.findall(r'EH-PER-\d{3}', text))
"
```

Expected: headings list matches the template above (minus any omitted sections); `eh_ids` count matches the *New in this pass* table row count.

- [ ] **Step 4: Ask user before committing**

Tell the user: *"Gaps doc written. May I commit?"* and wait for explicit yes.

- [ ] **Step 5: Commit**

```bash
git add docs/ocl/patient-registration-gaps.md
git commit -m "$(cat <<'EOF'
Add patient-registration coverage map

Documents which of the 27 candidate Person fields landed in CSV vs gaps,
plus negative PIH probe results so the next mapping pass doesn't re-search
the same terms.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

Expected: commit succeeds.

---

## Task 8: Update `docs/ocl/README.md`

**Files:**
- Modify: `docs/ocl/README.md`

- [ ] **Step 1: Add the coverage table row and bump totals**

In §What's here, locate the per-encounter coverage table (currently ends at the Home Visit row, then a `**Totals**` row showing 207/207).

Insert a new row immediately before the Totals row:

```
| Patient registration | `EH-PER` | <N> | <N> | `patient-registration-gaps.md` |
```

where `<N>` is the row count of `patient-registration-concepts.csv`.

Update the Totals row: add `<N>` to both 207 columns.

- [ ] **Step 2: Add the `eheza_field_path` extension note**

In §Concept CSV columns, locate the `eheza_field_path` row (says "Back-reference to the Elm type/field in `client/src/elm/Backend/Measurement/Model.elm`").

Append one sentence to that row's description: *"For patient-registration rows (`EH-PER-*`), this column points at the field in `client/src/elm/Backend/Person/Model.elm` instead."*

- [ ] **Step 3: Add the scope sentence**

In §Scope of this pass, in the *Encounter types* bullet, append after the Home Visit list: *"Plus a Patient Registration mapping group (`EH-PER`) sourced from the `Person` record — not an encounter type, but the demographic / contact / socioeconomic field set captured at first contact and reused by every subsequent encounter."*

- [ ] **Step 4: Add the methodology variant**

In §Methodology, after step 1 (Inventory), add a sub-paragraph:

> *For the Patient Registration pass, inventory walks the `Person` record's fields in `client/src/elm/Backend/Person/Model.elm` rather than encounter measurement aliases. Steps 2–4 (naming, PIH matching, confidence scoring) are unchanged.*

- [ ] **Step 5: Add the Person model bullet to "Where the Elm code lives"**

Insert a new bullet (alphabetical order if there is one; otherwise after `Backend/Measurement/Model.elm`):

> - `client/src/elm/Backend/Person/Model.elm` — the `Person` record (demographic / contact / socioeconomic fields captured at registration; the `eheza_field_path` source for `EH-PER-*` concepts)

- [ ] **Step 6: Render-check the README**

Run:
```bash
grep -n "EH-PER\|patient-registration\|Backend/Person/Model" docs/ocl/README.md
```

Expected: hits in the coverage table, the `eheza_field_path` row, the scope bullet, the methodology paragraph, and the "Where the Elm code lives" bullet.

- [ ] **Step 7: Ask user before committing**

Tell the user: *"README updated. May I commit?"* and wait for explicit yes.

- [ ] **Step 8: Commit**

```bash
git add docs/ocl/README.md
git commit -m "$(cat <<'EOF'
Document patient-registration mapping pass in OCL README

Adds coverage table row, totals bump, eheza_field_path extension note for
EH-PER-* rows pointing at Backend/Person/Model.elm, scope sentence,
methodology variant, and the Person model bullet under Where the Elm
code lives.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

Expected: commit succeeds.

---

## Task 9: Final verification across the whole `docs/ocl/` tree

**Files:** none modified; verification only.

- [ ] **Step 1: Re-run row-count parity for every encounter (regression check)**

```bash
for f in docs/ocl/*-concepts.csv; do
  base="${f%-concepts.csv}"
  c=$(($(wc -l < "$base-concepts.csv") - 1))
  m=$(($(wc -l < "$base-mappings.csv") - 1))
  printf "%-30s %3d concepts / %3d mappings\n" "$(basename $base)" "$c" "$m"
done
```

Expected: every line shows `N concepts / N mappings` (equal counts). The new `patient-registration` line is present.

- [ ] **Step 2: Confirm `csv_to_ocl_json.py` still emits valid JSON-Lines for the full set**

```bash
python3 docs/ocl/csv_to_ocl_json.py | python3 -c "
import sys, json
n = 0
for line in sys.stdin:
    json.loads(line)
    n += 1
print(f'parsed {n} json-lines, all valid')
"
```

Expected: `parsed N json-lines, all valid` where N matches the new total (previous total + 2 × patient-registration row count, since each concept also produces a mapping).

- [ ] **Step 3: Confirm `delta_upload.py` import path still works (no upload, just import-time sanity)**

```bash
python3 -c "import sys; sys.path.insert(0, 'docs/ocl'); import delta_upload; print('ok')"
```

Expected: `ok`. Any ImportError indicates a regression in `csv_to_ocl_json.py`'s public API (`concept_row` / `mapping_row`); investigate.

- [ ] **Step 4: Sanity-check the README totals math**

```bash
python3 -c "
import csv, pathlib
total_c = total_m = 0
for path in sorted(pathlib.Path('docs/ocl').glob('*-concepts.csv')):
    base = str(path)[:-len('-concepts.csv')]
    with open(base + '-concepts.csv') as f:
        total_c += sum(1 for _ in csv.DictReader(f))
    with open(base + '-mappings.csv') as f:
        total_m += sum(1 for _ in csv.DictReader(f))
print(f'totals: {total_c} concepts / {total_m} mappings')
"
```

Expected: matches the totals row in the README (Task 8 step 1).

- [ ] **Step 5: Confirm git status is clean**

```bash
git status -- docs/ocl/ docs/superpowers/
```

Expected: `nothing to commit, working tree clean` for `docs/ocl/` and `docs/superpowers/` paths. Any leftover changes mean a step was missed.

- [ ] **Step 6: Print final summary**

Tell the user: *"Patient registration mapping pass complete. `<N>` concepts in `patient-registration-concepts.csv`, `<N>` mappings, gaps doc written, README updated. Upload to OCL is out of scope for this PR — when ready, run the `delta_upload.py` workflow per `docs/ocl/README.md` §Uploading changes."*

No commit (verification only).
