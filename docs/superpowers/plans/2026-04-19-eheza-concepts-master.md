# E-Heza concepts master inventory — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Produce one canonical CSV (`docs/ocl/eheza-concepts-master.csv`) inventorying every clinical / patient-registration / care-delivery concept in 37 of E-Heza's `Backend/*/Model.elm` files, with `EHEZA-NNNN` ids assigned sequentially in a deterministic walk order.

**Architecture:** Pure documentation deliverable — three new files (master CSV, methodology doc, README section). No code, no schema changes, no edits to existing per-encounter `<x>-concepts.csv` / `<x>-mappings.csv` files. The build process uses a throwaway Python walker script (lives in `/tmp`, never committed) that parses each Elm `Backend/*/Model.elm` file and emits preliminary rows; an implementer then hand-verifies the output for datatype heuristics and name prettification.

**Tech Stack:** CSV (de-facto `ocldev/ocl_csv_to_json_flex` schema family — though this file is intentionally not OCL-import-ready), Python 3 for the scratch walker (uses regex on Elm source — no formal Elm parser needed for this depth of walk).

**Spec:** `docs/superpowers/specs/2026-04-19-eheza-concepts-master-design.md`

**Conventions enforced throughout:**
- Per global preference: **always ask the user before running `git commit` or `git push`** — every commit step below is "ask, then commit if approved".
- Per CLAUDE.md: every commit message ends with `[ci skip]` (after the Co-Authored-By line).
- Per spec § Schema: 6 columns (`id`, `name`, `eheza_field_path`, `source_module`, `elm_construct`, `datatype`). No more, no fewer. RFC 4180 quoting; UTF-8.
- Per spec § Walk methodology: walk order is *files alphabetical → declarations source-order → members source-order*. ID assignment is *sequential in walk order*. **Once committed, IDs are immutable.**
- Per spec § Build process: scratch walker tool lives in `/tmp/eheza-walker.py` and is NOT committed. Hand-verification is required between walk output and commit.

---

## File Structure

**Create:**
- `docs/ocl/eheza-concepts-master.csv` — the inventory itself, ~1000–1500 rows.
- `docs/ocl/eheza-concepts-master.md` — methodology doc (purpose, schema, walk rules, edge cases, process discipline for future Elm changes).

**Modify:**
- `docs/ocl/README.md` — new top-level section after *Encounter types not (yet) covered* introducing the master file. Coverage table is NOT updated (master is a separate artefact, not part of the per-encounter mapping structure).

**Already done before this plan starts:**
- Branch `docs/eheza-concepts-master` created off `docs/ocl-pih-mappings` (controller did this during brainstorming).
- Spec at `docs/superpowers/specs/2026-04-19-eheza-concepts-master-design.md` is uncommitted in working tree.

**Working / scratch files (uncommitted, lives in `/tmp`):**
- `/tmp/eheza-walker.py` — Python script that walks the 37 Elm files and emits preliminary CSV.
- `/tmp/eheza-preliminary.csv` — walker output before hand-verification.
- `/tmp/eheza-walk-summary.txt` — per-file row counts for sanity-checking against `grep` counts of Elm `type` declarations.

**No code in the repo changes.** No `.gitignore` updates. No edits to existing per-encounter CSVs.

---

## Task 0: Commit spec + plan (controller-handled)

**Files:**
- Stage: `docs/superpowers/specs/2026-04-19-eheza-concepts-master-design.md`
- Stage: `docs/superpowers/plans/2026-04-19-eheza-concepts-master.md`

- [ ] **Step 1: Verify both files are uncommitted**

Run:
```bash
git status -- docs/superpowers/
```

Expected: both files listed as untracked. If they're already committed, skip this task entirely.

- [ ] **Step 2: Ask user before committing**

Tell the user: *"Spec and plan are uncommitted. May I commit them as one combined commit?"* Wait for explicit yes/no.

- [ ] **Step 3: Commit**

```bash
git add docs/superpowers/specs/2026-04-19-eheza-concepts-master-design.md \
        docs/superpowers/plans/2026-04-19-eheza-concepts-master.md
git commit -m "$(cat <<'EOF'
Add eheza-concepts-master inventory spec and implementation plan

Spec scopes a canonical inventory of every concept E-Heza captures,
drawn from a deterministic walk of 37 Backend/*/Model.elm files.
Plan decomposes into 9 tasks: methodology doc → walker script →
preliminary CSV → walker sanity-check → hand-verification → final
validation → CSV commit → README update → final review.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

Expected: commit succeeds.

---

## Task 1: Write the methodology doc

**Files:**
- Create: `docs/ocl/eheza-concepts-master.md`

This doc gets committed BEFORE the CSV so reviewers of the CSV commit can see the rules the data was built against.

- [ ] **Step 1: Look at one existing per-encounter `<x>-gaps.md` to copy section voice and formatting**

Run:
```bash
ls docs/ocl/*-gaps.md
```

Pick `docs/ocl/homevisit-gaps.md` or `docs/ocl/nutrition-gaps.md` and read it end-to-end. Match the voice (compact, factual, code-fences for commands, tables for structured data) but write FRESH content per the spec.

- [ ] **Step 2: Write `docs/ocl/eheza-concepts-master.md`**

Create the file with these sections (in this order):

1. **Header paragraph** — what this file is, what `eheza-concepts-master.csv` is, and what relationship (if any) it has to the existing `<x>-concepts.csv` / `<x>-mappings.csv` files. Be explicit: *no relationship, no shared ID space, no cross-references*. The master is the future search baseline for cross-org mapping passes (UVL-Burundi is a known upcoming consumer); the per-encounter PIH-mapped artefacts continue to live their own life unchanged.

2. **§ Schema** — reproduce the 6-column table from spec §Schema verbatim (columns: `id`, `name`, `eheza_field_path`, `source_module`, `elm_construct`, `datatype`).

3. **§ Datatype derivation rules** — reproduce the datatype mapping table from spec §Schema verbatim (the table that maps Elm types → OpenMRS-flavoured datatypes).

4. **§ Walk methodology** — reproduce the walk order rules + ID assignment rule + external references rules + edge cases from spec §Walk methodology.

5. **§ Inventory pass metadata** — record the date the walk was performed and the git SHA of the source tree at walk time. This is filled in during Task 3 step 4 once the walker has run; for now leave a placeholder bullet:

   ```markdown
   - **Walk date**: <YYYY-MM-DD>
   - **Source tree SHA**: <git rev-parse HEAD output>
   - **Walker tool**: scratch Python script at `/tmp/eheza-walker.py` (not committed; see *Build process* below)
   ```

   These get filled in just before commit, not now.

6. **§ Process discipline for future Elm changes** — one short paragraph stating the discipline (verbatim from spec §Repository changes item 2):

   > If you add a new measurement type alias, new record field, or new union constructor to an in-scope module (any of the 37 listed in *Scope*), append a row to `eheza-concepts-master.csv` with the next available `EHEZA-NNNN` id as part of the same PR. The walk order is only used to assign IDs during the *initial* inventory pass; net-new entries after that get the next free id regardless of where they fall in walk order.

7. **§ Scope** — the 37 in-scope files + the 4 excluded (`Dashboard`, `ResilienceMessage`, `ResilienceSurvey`, `PatientRecord`) + a one-sentence rationale for the exclusion.

8. **§ Explicit non-relationship to per-encounter CSVs** — short paragraph: master and per-encounter files have separate ID spaces, separate purposes, no cross-references. Future cross-org mapping passes consume the master, NOT the per-encounter files. (This is important enough to be its own section so reviewers can find it.)

- [ ] **Step 3: Render-check the file**

Run:
```bash
python3 -c "
import re, pathlib
text = pathlib.Path('docs/ocl/eheza-concepts-master.md').read_text()
print('headings:', re.findall(r'^##+ .*', text, re.M))
print('eh_per_count:', len(re.findall(r'EH-PER', text)))
print('eheza_id_format:', re.findall(r'EHEZA-\d{4}', text)[:3])
"
```

Expected:
- `headings` includes `## Schema`, `## Datatype derivation rules`, `## Walk methodology`, `## Inventory pass metadata`, `## Process discipline for future Elm changes`, `## Scope`, `## Explicit non-relationship to per-encounter CSVs` (in that order).
- `eh_per_count` is 0 (the methodology doc must NOT cite previous PIH ids — that violates the spec's no-relationship rule).
- `eheza_id_format` shows the 4-digit padded format in at least 1–3 example mentions.

- [ ] **Step 4: Ask user before committing**

Tell the user: *"Methodology doc written. May I commit?"* Wait for yes.

- [ ] **Step 5: Commit**

```bash
git add docs/ocl/eheza-concepts-master.md
git commit -m "$(cat <<'EOF'
Add eheza-concepts-master methodology doc

Documents purpose, schema, datatype derivation rules, walk methodology,
edge cases, and the process discipline for future Elm changes. Commits
the rules before the CSV so reviewers of the data commit can verify the
walker output against the documented methodology.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

Expected: commit succeeds.

---

## Task 2: Write the scratch walker script

**Files:**
- Create: `/tmp/eheza-walker.py` (NOT committed, not in repo)

The walker reads each of the 37 in-scope Elm files and emits one CSV row per concept. It uses regex on Elm source — full AST parsing isn't needed because Elm `Model.elm` files have very stylized type declarations.

- [ ] **Step 1: Confirm the 37 in-scope files and write them to a fixed list inside the walker**

The 37 files (alphabetical by repo-relative path):

```
client/src/elm/Backend/AcuteIllnessActivity/Model.elm
client/src/elm/Backend/AcuteIllnessEncounter/Model.elm
client/src/elm/Backend/ChildScoreboardActivity/Model.elm
client/src/elm/Backend/ChildScoreboardEncounter/Model.elm
client/src/elm/Backend/Clinic/Model.elm
client/src/elm/Backend/Counseling/Model.elm
client/src/elm/Backend/EducationSession/Model.elm
client/src/elm/Backend/FamilyEncounterParticipant/Model.elm
client/src/elm/Backend/FamilyNutritionActivity/Model.elm
client/src/elm/Backend/FamilyNutritionEncounter/Model.elm
client/src/elm/Backend/HIVActivity/Model.elm
client/src/elm/Backend/HIVEncounter/Model.elm
client/src/elm/Backend/HealthCenter/Model.elm
client/src/elm/Backend/HomeVisitActivity/Model.elm
client/src/elm/Backend/HomeVisitEncounter/Model.elm
client/src/elm/Backend/IndividualEncounterParticipant/Model.elm
client/src/elm/Backend/Measurement/Model.elm
client/src/elm/Backend/NCDActivity/Model.elm
client/src/elm/Backend/NCDEncounter/Model.elm
client/src/elm/Backend/Nurse/Model.elm
client/src/elm/Backend/NutritionActivity/Model.elm
client/src/elm/Backend/NutritionEncounter/Model.elm
client/src/elm/Backend/ParticipantConsent/Model.elm
client/src/elm/Backend/Person/Model.elm
client/src/elm/Backend/PmtctParticipant/Model.elm
client/src/elm/Backend/PrenatalActivity/Model.elm
client/src/elm/Backend/PrenatalEncounter/Model.elm
client/src/elm/Backend/Relationship/Model.elm
client/src/elm/Backend/Session/Model.elm
client/src/elm/Backend/StockUpdate/Model.elm
client/src/elm/Backend/TraceContact/Model.elm
client/src/elm/Backend/TuberculosisActivity/Model.elm
client/src/elm/Backend/TuberculosisEncounter/Model.elm
client/src/elm/Backend/Village/Model.elm
client/src/elm/Backend/WellChildActivity/Model.elm
client/src/elm/Backend/WellChildEncounter/Model.elm
```

(36 listed — the 37th is missing because some `Backend/*/Model.elm` files don't exist in the project. Cross-check by running:
```bash
ls client/src/elm/Backend/ | while read d; do
  [ -f "client/src/elm/Backend/$d/Model.elm" ] && echo "client/src/elm/Backend/$d/Model.elm"
done | grep -v -E "Dashboard|ResilienceMessage|ResilienceSurvey|PatientRecord" | sort
```

If the count differs from 37, update the walker's IN_SCOPE list to match what actually exists. The spec's headline number "37" is approximate — what matters is that all clinical / Person / care-delivery `Backend/*/Model.elm` files except the 4 named exclusions are walked.)

- [ ] **Step 2: Write the walker script**

Create `/tmp/eheza-walker.py` with this exact content:

```python
#!/usr/bin/env python3
"""Walk in-scope Backend/*/Model.elm files and emit preliminary master inventory CSV.

Output to stdout: CSV with header `id,name,eheza_field_path,source_module,elm_construct,datatype`.
Per-file summary to stderr.

Usage:
    python3 /tmp/eheza-walker.py > /tmp/eheza-preliminary.csv 2> /tmp/eheza-walk-summary.txt
"""
import csv
import os
import re
import sys

REPO = "/var/www/html/ihangane"

EXCLUDED = {"Dashboard", "ResilienceMessage", "ResilienceSurvey", "PatientRecord"}


def in_scope_files():
    backend = os.path.join(REPO, "client/src/elm/Backend")
    out = []
    for d in sorted(os.listdir(backend)):
        if d in EXCLUDED:
            continue
        path = os.path.join(backend, d, "Model.elm")
        if os.path.isfile(path):
            out.append(os.path.relpath(path, REPO))
    return out


# Strip Elm comments (line + block) before regex parsing.
def strip_comments(src: str) -> str:
    # Block comments {- ... -} (non-nested handling — Elm allows nested but rare in Model.elm)
    src = re.sub(r"\{-[\s\S]*?-\}", "", src)
    # Line comments
    src = re.sub(r"--[^\n]*", "", src)
    return src


# Mechanical name prettifier: split CamelCase on case boundaries, lowercase all but first letter.
# E.g., "BasicVitalsValue" -> "Basic vitals value", "BreastfeedingExclusively" -> "Breastfeeding exclusively"
def prettify(name: str) -> str:
    # Split on lower→upper boundary: insert space
    s = re.sub(r"(?<=[a-z0-9])(?=[A-Z])", " ", name)
    # Also split on consecutive caps followed by lower (e.g., "HIVStatus" -> "HIV Status")
    s = re.sub(r"(?<=[A-Z])(?=[A-Z][a-z])", " ", s)
    parts = s.split()
    if not parts:
        return name
    return parts[0][0].upper() + parts[0][1:].lower() + (
        " " + " ".join(p.lower() for p in parts[1:]) if len(parts) > 1 else ""
    )


# Heuristic Elm-type → OpenMRS-style datatype mapping.
# Receives a stripped-down representation of the type expression on the RHS of `:`.
def derive_datatype(type_expr: str) -> str:
    t = type_expr.strip()
    # Strip Maybe X / List X / EverySet X wrappers and recurse on the inner type
    for wrapper in ("Maybe ", "List ", "EverySet ", "Set ", "AssocList.Dict "):
        if t.startswith(wrapper):
            return derive_datatype(t[len(wrapper):].strip())
    # Strip outer parens
    if t.startswith("(") and t.endswith(")"):
        return derive_datatype(t[1:-1].strip())
    # Primitive checks (anchored / start-of-string match against known Elm types)
    if t in ("Int", "Float"):
        return "Numeric"
    if t == "String":
        return "Text"
    if t == "Bool":
        return "Boolean"
    if t in ("NominalDate", "Date", "Posix"):
        return "Date"
    # EntityId X -> Text (UUID-style identifier)
    if t.startswith("EntityId ") or t.endswith("Id"):
        return "Text"
    # Json.Decode.Value / Json.Encode.Value -> N/A
    if t in ("Value", "Json.Decode.Value", "Json.Encode.Value"):
        return "N/A"
    # Tuple types -> N/A
    if t.startswith("("):
        return "N/A"
    # Capitalised single-word identifier with no qualifier/dot -> assume custom union type from same module -> Coded
    if re.match(r"^[A-Z][A-Za-z0-9_]*$", t):
        return "Coded"
    # Anything else (qualified types, parametric records, function types, etc.) -> N/A
    return "N/A"


# Walk a single Elm file and yield rows.
# Each row is a tuple (name, eheza_field_path, source_module, elm_construct, datatype).
# IDs are assigned later by the caller (so they're sequential across all files).
def walk_file(path: str):
    src = open(os.path.join(REPO, path)).read()
    src = strip_comments(src)
    rows = []
    # Find every top-level `type alias Foo = ...` and `type Foo = ...` declaration in source order.
    # `^type\s+(alias\s+)?(\w+)\s*(?:[\w\s]*?)\s*=\s*(.*?)(?=^(?:type\s|\w+\s*:|\Z))`
    # We scan the file linearly to keep source order.
    decl_pattern = re.compile(
        r"^type(\s+alias)?\s+([A-Z]\w*)(?:\s+[a-z]\w*)*\s*=\s*",
        re.MULTILINE,
    )
    matches = list(decl_pattern.finditer(src))
    for i, m in enumerate(matches):
        is_alias = bool(m.group(1))
        type_name = m.group(2)
        body_start = m.end()
        body_end = matches[i + 1].start() if i + 1 < len(matches) else len(src)
        body = src[body_start:body_end].strip()
        # Trim trailing top-level value declarations within body (rare, but Elm allows things
        # like a trailing function definition right before the next type). Heuristically cut
        # at the first line that looks like a function type signature (`name : Type`).
        cut = re.search(r"^\w+\s*:\s", body, re.MULTILINE)
        if cut:
            body = body[:cut.start()].strip()

        if is_alias and body.startswith("{"):
            # Record type alias
            rows.append((prettify(type_name), type_name, path, "record_type", "N/A"))
            # Parse fields from inside the braces. Record body is `{ field : Type, field : Type, ... }`.
            inner = body[1:body.rfind("}")].strip()
            for field, type_expr in parse_record_fields(inner):
                rows.append((
                    prettify(field),
                    f"{type_name}.{field}",
                    path,
                    "record_field",
                    derive_datatype(type_expr),
                ))
        elif is_alias:
            # Non-record type alias (e.g., `type alias FooId = EntityId Foo`)
            rows.append((prettify(type_name), type_name, path, "record_type", "N/A"))
        else:
            # Union type
            rows.append((prettify(type_name), type_name, path, "union_type", "Coded"))
            # Parse constructors (split on `|` at depth 0)
            for ctor_name in parse_union_constructors(body):
                rows.append((
                    prettify(ctor_name),
                    f"{type_name}.{ctor_name}",
                    path,
                    "union_constructor",
                    "N/A",
                ))
    return rows


def parse_record_fields(inner: str):
    """Yield (field_name, type_expression) pairs from a record body (without surrounding braces)."""
    # Split on commas at depth 0 (respecting parens/braces).
    depth_paren = 0
    depth_brace = 0
    parts = []
    cur = []
    for c in inner:
        if c == "(":
            depth_paren += 1
        elif c == ")":
            depth_paren -= 1
        elif c == "{":
            depth_brace += 1
        elif c == "}":
            depth_brace -= 1
        if c == "," and depth_paren == 0 and depth_brace == 0:
            parts.append("".join(cur).strip())
            cur = []
        else:
            cur.append(c)
    if cur:
        parts.append("".join(cur).strip())

    for p in parts:
        if not p:
            continue
        # Field declaration: `name : Type`
        m = re.match(r"^([a-z]\w*)\s*:\s*(.+)$", p, re.DOTALL)
        if m:
            yield m.group(1), m.group(2).strip()


def parse_union_constructors(body: str):
    """Yield constructor identifiers from a union type body (without leading `=`)."""
    depth_paren = 0
    parts = []
    cur = []
    for c in body:
        if c == "(":
            depth_paren += 1
        elif c == ")":
            depth_paren -= 1
        if c == "|" and depth_paren == 0:
            parts.append("".join(cur).strip())
            cur = []
        else:
            cur.append(c)
    if cur:
        parts.append("".join(cur).strip())

    for p in parts:
        if not p:
            continue
        # Constructor: `Name [args...]` — take the first identifier
        m = re.match(r"^([A-Z]\w*)", p)
        if m:
            yield m.group(1)


def main():
    files = in_scope_files()
    print(f"Walking {len(files)} files", file=sys.stderr)

    writer = csv.writer(sys.stdout)
    writer.writerow(["id", "name", "eheza_field_path", "source_module", "elm_construct", "datatype"])

    next_id = 1
    for path in files:
        rows_before = next_id
        rows = walk_file(path)
        for name, fpath, mod, construct, dt in rows:
            writer.writerow([f"EHEZA-{next_id:04d}", name, fpath, mod, construct, dt])
            next_id += 1
        emitted = next_id - rows_before
        print(f"  {path}: {emitted} rows (ids {rows_before:04d}..{next_id - 1:04d})", file=sys.stderr)

    print(f"Total rows: {next_id - 1}", file=sys.stderr)


if __name__ == "__main__":
    main()
```

- [ ] **Step 3: Confirm the script is syntactically valid Python**

```bash
python3 -m py_compile /tmp/eheza-walker.py && echo "ok"
```

Expected: prints `ok`. Any SyntaxError means the script has a typo — fix before proceeding.

No commit (scratch file only — `/tmp/`).

---

## Task 3: Run the walker and produce preliminary CSV

**Files:**
- Write: `/tmp/eheza-preliminary.csv` (scratch)
- Write: `/tmp/eheza-walk-summary.txt` (scratch)

- [ ] **Step 1: Run the walker**

```bash
cd /var/www/html/ihangane && python3 /tmp/eheza-walker.py \
  > /tmp/eheza-preliminary.csv 2> /tmp/eheza-walk-summary.txt
echo "exit=$?"
```

Expected: `exit=0`. Any non-zero exit means the walker crashed (likely on a regex edge case in some `Model.elm`); read stderr to find the file, fix the walker (Task 2 step 2), re-run.

- [ ] **Step 2: Inspect the per-file summary**

```bash
cat /tmp/eheza-walk-summary.txt
```

Expected output shape:
```
Walking N files
  client/src/elm/Backend/AcuteIllnessActivity/Model.elm: M rows (ids 0001..00MM)
  ...
Total rows: NNNN
```

The total should be in the rough range 1000–1500 (per spec estimate). If it's wildly off (say <500 or >3000), the walker has a bug — fix and re-run.

- [ ] **Step 3: Cross-check walker output against grep counts of `type` declarations per file**

For each file in the walker's IN_SCOPE list, count top-level `type` and `type alias` declarations and compare against the walker's per-file row count. The walker's row count should equal `(# record types) + (# fields across all records) + (# union types) + (# constructors across all unions) + (# non-record type aliases)`.

A quick sanity-only check (just total declarations, doesn't verify field/constructor counts):

```bash
total_decl=0
for f in $(awk -F: '/Model.elm:/ {print $1}' /tmp/eheza-walk-summary.txt | sed 's/^ *//'); do
  count=$(grep -cE "^type(\s+alias)?\s+[A-Z]" "$f")
  total_decl=$((total_decl + count))
done
echo "total top-level type declarations across in-scope files: $total_decl"
echo "walker emitted record_type/union_type rows:"
awk -F, 'NR>1 && ($5=="record_type" || $5=="union_type") {n++} END {print n}' /tmp/eheza-preliminary.csv
```

Expected: the two numbers should match. If they don't, the walker missed (or doubled) some declarations — fix and re-run.

- [ ] **Step 4: Sample 10 random rows and read them back against the source**

```bash
shuf -n 10 /tmp/eheza-preliminary.csv | column -t -s,
```

For each sampled row, open the `source_module` and confirm:
- The `eheza_field_path` exists in that file
- The `elm_construct` matches what's actually there
- The `datatype` looks plausible per the derivation rules

If any row is wrong, the walker has a bug — fix and re-run.

- [ ] **Step 5: Snapshot row count for downstream tasks**

Note the total row count from step 2 — Task 6 step 5 checks the master CSV has the same count.

No commit (scratch files only).

---

## Task 4: Hand-verification pass

**Files:**
- Read: `/tmp/eheza-preliminary.csv`
- Write: `/tmp/eheza-master.csv` (corrected version, still scratch)

The walker's heuristics are imperfect. Common things to fix during hand-verification:

- **Datatype heuristic missed a custom enum stored as `String`** — e.g., a field typed `String` that's actually a stringified enum. Datatype should be `Text` per the rules (we don't have visibility into clinical semantics), so leave as `Text` — this is a documented limitation, not a fix.
- **Acronym capitalisation** — the prettifier turns `HIVStatus` into `Hiv status`, but the canonical spelling is `HIV status`. Manually fix occurrences of common acronyms: `HIV`, `MUAC`, `BMI`, `ANC`, `BP`, `HCT`, `HMIS`, `GPS`, `NCD`, `TB`, `RDT`, `IUD`, `OPV`, `BCG`, `DPT`, `IPV`, `MR`, `PCV`.
- **Walker missed a type alias due to multi-line layout** — Elm allows declarations to span multiple lines with unusual whitespace. If the `cross-check` in Task 3 step 3 surfaced a count mismatch, the missing type(s) need to be hand-added with the correct ids slotted in walk-order.
- **Constructor argument types containing `|`** — the union-constructor splitter might mis-split if a constructor argument is itself a union (e.g., `BarCtor (Foo | Bar)` — illegal in Elm but the splitter could be tripped by parenthesised types). Walker uses depth-tracked `|` splitting; if a row looks wrong, sanity-check by hand.

- [ ] **Step 1: Copy preliminary to working file**

```bash
cp /tmp/eheza-preliminary.csv /tmp/eheza-master.csv
```

- [ ] **Step 2: Apply acronym fix-ups in bulk**

For each acronym in the list above, run a `sed` replacement that targets only the `name` column (the second CSV column) — NOT the `eheza_field_path` (which must stay verbatim Elm) or `source_module`. Pattern: replace `Hiv ` with `HIV ` only when it appears as a CamelCase-prettified word at the start of (or within) a name field, not in any other column.

A safer approach (Python rather than sed, to avoid touching other columns):

```bash
python3 <<'EOF'
import csv
ACRONYMS = {
    "Hiv": "HIV", "Muac": "MUAC", "Bmi": "BMI", "Anc": "ANC", "Bp": "BP",
    "Hct": "HCT", "Hmis": "HMIS", "Gps": "GPS", "Ncd": "NCD", "Tb": "TB",
    "Rdt": "RDT", "Iud": "IUD", "Opv": "OPV", "Bcg": "BCG", "Dpt": "DPT",
    "Ipv": "IPV", "Mr": "MR", "Pcv": "PCV",
}
out_rows = []
with open("/tmp/eheza-master.csv") as f:
    reader = csv.DictReader(f)
    fieldnames = reader.fieldnames
    for r in reader:
        n = r["name"]
        # Replace as standalone words (preceded/followed by space or string boundary)
        for wrong, right in ACRONYMS.items():
            n = n.replace(f" {wrong} ", f" {right} ")
            if n.startswith(f"{wrong} "):
                n = right + n[len(wrong):]
            if n.endswith(f" {wrong}"):
                n = n[:-len(wrong)] + right
            if n == wrong:
                n = right
        r["name"] = n
        out_rows.append(r)
with open("/tmp/eheza-master.csv", "w", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=fieldnames)
    writer.writeheader()
    writer.writerows(out_rows)
print(f"applied acronym fix-ups across {len(out_rows)} rows")
EOF
```

Expected: prints the row count.

- [ ] **Step 3: Spot-check 20 random rows post-prettification**

```bash
shuf -n 20 /tmp/eheza-master.csv | column -t -s,
```

Read each row's `name` and `eheza_field_path` together. The `name` should be a readable English-ish version of the identifier in `eheza_field_path`. If anything reads badly, it's either an acronym we missed (add to the dictionary in step 2 and re-run) or a true edge case (manually fix the row).

- [ ] **Step 4: Per-file row-count diff sanity** (catches walker bugs)

```bash
echo "preliminary per-file:"
awk -F, 'NR>1 {print $4}' /tmp/eheza-preliminary.csv | sort | uniq -c | sort -rn
echo ""
echo "post-verification per-file:"
awk -F, 'NR>1 {print $4}' /tmp/eheza-master.csv | sort | uniq -c | sort -rn
```

Expected: the two should be identical (we only edited `name`, not added/removed rows). Any difference means the verification accidentally added or dropped rows — investigate.

No commit (scratch files only). The next task validates and then copies to `docs/ocl/`.

---

## Task 5: Run the formal validation suite

**Files:**
- Read: `/tmp/eheza-master.csv`

All 6 checks from spec §Validation. Any failure means going back to Task 4 (or, if a walker bug is implicated, Task 2).

- [ ] **Step 1: No duplicate `id`**

```bash
awk -F, 'NR>1 {print $1}' /tmp/eheza-master.csv | sort | uniq -d
```

Expected: empty output. Any output means duplicate IDs — almost certainly a walker bug.

- [ ] **Step 2: IDs contiguous starting at `EHEZA-0001`**

```bash
python3 -c "
import csv
with open('/tmp/eheza-master.csv') as f:
    ids = [r['id'] for r in csv.DictReader(f)]
nums = [int(i.split('-', 1)[1]) for i in ids]
expected = list(range(1, len(nums) + 1))
print('rows=', len(nums))
print('first=', ids[0], 'last=', ids[-1])
print('contiguous=', nums == expected)
print('first gap=', next((i for i, (a, b) in enumerate(zip(nums, expected)) if a != b), 'none'))
"
```

Expected: `contiguous= True` and `first gap= none`. If not, IDs are not sequential (walker bug, or Task 4 dropped/added rows).

- [ ] **Step 3: Every `eheza_field_path` resolves to real Elm source**

```bash
python3 <<'EOF'
import csv, re
unresolved = []
src_cache = {}
def src(path):
    if path not in src_cache:
        with open(path) as f:
            src_cache[path] = f.read()
    return src_cache[path]
with open("/tmp/eheza-master.csv") as f:
    for r in csv.DictReader(f):
        path = r["source_module"]
        ref = r["eheza_field_path"]
        construct = r["elm_construct"]
        text = src(path)
        if construct in ("record_type", "union_type"):
            ok = re.search(rf"^type(\s+alias)?\s+{re.escape(ref)}\b", text, re.MULTILINE) is not None
        elif construct == "record_field":
            type_name, field = ref.split(".", 1)
            ok = re.search(rf"\b{re.escape(field)}\s*:", text) is not None
        elif construct == "union_constructor":
            type_name, ctor = ref.split(".", 1)
            ok = re.search(rf"\b{re.escape(ctor)}\b", text) is not None
        else:
            ok = False
        if not ok:
            unresolved.append((r["id"], ref, path, construct))
print(f"unresolved: {len(unresolved)}")
for u in unresolved[:20]:
    print(" ", u)
EOF
```

Expected: `unresolved: 0`. Any unresolved row means either a walker bug or a typo introduced during Task 4. Fix and re-validate.

- [ ] **Step 4: Every `source_module` is in the in-scope allowlist**

```bash
python3 <<'EOF'
import csv, os
EXCLUDED = {"Dashboard", "ResilienceMessage", "ResilienceSurvey", "PatientRecord"}
allowlist = set()
backend = "client/src/elm/Backend"
for d in os.listdir(backend):
    if d in EXCLUDED:
        continue
    p = os.path.join(backend, d, "Model.elm")
    if os.path.isfile(p):
        allowlist.add(p)
bad = set()
with open("/tmp/eheza-master.csv") as f:
    for r in csv.DictReader(f):
        if r["source_module"] not in allowlist:
            bad.add(r["source_module"])
print(f"allowlist size: {len(allowlist)}, out-of-allowlist source_modules: {len(bad)}")
for b in sorted(bad):
    print(" ", b)
EOF
```

Expected: `out-of-allowlist source_modules: 0`. Any output means the walker emitted a row for an excluded file or a non-existent file.

- [ ] **Step 5: `datatype` values are in the allowed set**

```bash
awk -F, 'NR>1 {print $6}' /tmp/eheza-master.csv | sort -u
```

Expected: only `Boolean`, `Coded`, `Date`, `N/A`, `Numeric`, `Text` (alphabetical). Any other value is a walker bug.

- [ ] **Step 6: Row count sanity**

```bash
echo "row count: $(($(wc -l < /tmp/eheza-master.csv) - 1))"
```

Expected: in the range 1000–1500 (spec estimate). If wildly outside, investigate (likely a walker bug missed an entire file or double-counted).

No commit (still in /tmp). Move to Task 6 to place the file in the repo.

---

## Task 6: Move the master CSV into the repo

**Files:**
- Create: `docs/ocl/eheza-concepts-master.csv`

- [ ] **Step 1: Copy the validated file into the repo**

```bash
cp /tmp/eheza-master.csv /var/www/html/ihangane/docs/ocl/eheza-concepts-master.csv
echo "copied; row count: $(($(wc -l < /var/www/html/ihangane/docs/ocl/eheza-concepts-master.csv) - 1))"
```

Expected: prints the same row count from Task 5 step 6.

- [ ] **Step 2: Confirm the file parses as valid CSV from the repo path**

```bash
python3 -c "
import csv
with open('/var/www/html/ihangane/docs/ocl/eheza-concepts-master.csv') as f:
    rows = list(csv.DictReader(f))
print(f'rows={len(rows)}, cols={list(rows[0].keys())}')
print(f'first id: {rows[0][\"id\"]}, last id: {rows[-1][\"id\"]}')
"
```

Expected: `rows=NNNN`, columns are `['id', 'name', 'eheza_field_path', 'source_module', 'elm_construct', 'datatype']`, first id is `EHEZA-0001`, last id matches the walker total.

- [ ] **Step 3: Update the methodology doc's *Inventory pass metadata* section with real values**

Open `docs/ocl/eheza-concepts-master.md` and replace the placeholder bullets in the *Inventory pass metadata* section with real values:

```bash
SHA=$(git -C /var/www/html/ihangane rev-parse HEAD)
DATE=$(date -u +%Y-%m-%d)
ROWS=$(($(wc -l < /var/www/html/ihangane/docs/ocl/eheza-concepts-master.csv) - 1))
echo "Replace placeholders with:"
echo "  walk_date: $DATE"
echo "  source_sha: $SHA"
echo "  row_count: $ROWS"
```

Edit `docs/ocl/eheza-concepts-master.md` to substitute these values. Final markdown should look like:

```markdown
- **Walk date**: <DATE from above>
- **Source tree SHA**: <SHA from above>
- **Walker tool**: scratch Python script at `/tmp/eheza-walker.py` (not committed; see *Build process* below)
- **Initial row count**: <ROWS from above>
```

- [ ] **Step 4: Stage both files (CSV + methodology doc update) for the commit**

```bash
cd /var/www/html/ihangane && git status -- docs/ocl/eheza-concepts-master.csv docs/ocl/eheza-concepts-master.md
```

Expected: CSV is untracked (new), methodology doc is modified.

- [ ] **Step 5: Ask user before committing**

Tell the user:
*"Master CSV ready (`docs/ocl/eheza-concepts-master.csv`, NNNN rows). Methodology doc updated with walk-date/source-SHA/row-count. May I commit both as one combined commit?"* Wait for yes.

- [ ] **Step 6: Commit**

```bash
git add docs/ocl/eheza-concepts-master.csv docs/ocl/eheza-concepts-master.md
git commit -m "$(cat <<'EOF'
Add E-Heza concepts master inventory CSV

NNNN rows enumerating every concept in 37 in-scope Backend/*/Model.elm
files (everything except Dashboard, ResilienceMessage, ResilienceSurvey,
PatientRecord). Sequential EHEZA-NNNN ids in deterministic walk order;
ids are now immutable (future Elm changes append rows with the next
free id, regardless of source-order position).

The master is a clean canonical inventory with no relationship to the
existing per-encounter PIH-mapped <x>-concepts.csv / <x>-mappings.csv
files — separate ID space, separate purpose. Future cross-org mapping
passes (UVL-Burundi is the known upcoming consumer) consume the master
as the search baseline.

Methodology doc updated with the actual walk date, source-tree SHA at
walk time, and the final row count.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

Replace `NNNN` in both the message body and the headline with the real row count from step 1. Expected: commit succeeds.

---

## Task 7: Update `docs/ocl/README.md`

**Files:**
- Modify: `docs/ocl/README.md`

- [ ] **Step 1: Locate the insertion point**

The new section goes after *Encounter types not (yet) covered* and before *Target OCL source*.

```bash
grep -n "Encounter types not\|Target OCL source" docs/ocl/README.md
```

Expected: two line numbers; the new section gets inserted between them.

- [ ] **Step 2: Insert the new section**

Add this section to `docs/ocl/README.md` immediately before the `## Target OCL source` heading (use Edit to insert above that heading):

```markdown
## Master inventory — `eheza-concepts-master.csv`

Separate from the per-encounter PIH-mapped pairs above, `eheza-concepts-master.csv`
is a canonical inventory of every concept E-Heza captures, drawn from a deterministic
walk of 37 of the 40 `client/src/elm/Backend/*/Model.elm` files (excluded:
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
```

- [ ] **Step 3: Verify the edit landed**

```bash
grep -nE "Master inventory|eheza-concepts-master" docs/ocl/README.md
```

Expected: at least one line citing the new section, plus mentions of `eheza-concepts-master.csv` / `eheza-concepts-master.md`. Also confirm by reading the file back around the insertion point that the section sits between *Encounter types not (yet) covered* and *Target OCL source*.

- [ ] **Step 4: Ask user before committing**

Tell the user: *"README updated with master-inventory section. May I commit?"* Wait for yes.

- [ ] **Step 5: Commit**

```bash
git add docs/ocl/README.md
git commit -m "$(cat <<'EOF'
Document eheza-concepts-master inventory in OCL README

Adds a top-level section after "Encounter types not (yet) covered"
introducing the master CSV, explaining its purpose as a future
cross-org mapping baseline, and pointing at the methodology doc.
Coverage table is intentionally not updated — the master file is
not part of the per-encounter mapping structure.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

Expected: commit succeeds.

---

## Task 8: Final verification

**Files:** none modified; verification only.

- [ ] **Step 1: Confirm the branch has 5 commits ahead of `docs/ocl-pih-mappings`**

```bash
git log --oneline docs/eheza-concepts-master ^docs/ocl-pih-mappings
```

Expected: 5 commit lines, in order (newest first):
```
README update
master CSV + methodology doc metadata
methodology doc
spec + plan
```

(Or 4 if Task 0 was skipped because the spec/plan were already committed.)

- [ ] **Step 2: Confirm git status is clean**

```bash
git status -- docs/ocl/ docs/superpowers/
```

Expected: `nothing to commit, working tree clean` for both subdirectories.

- [ ] **Step 3: Re-validate the master CSV in its committed location**

```bash
cd /var/www/html/ihangane && python3 -c "
import csv
with open('docs/ocl/eheza-concepts-master.csv') as f:
    rows = list(csv.DictReader(f))
ids = [r['id'] for r in rows]
print(f'rows={len(rows)}')
print(f'columns={list(rows[0].keys())}')
print(f'first/last id: {ids[0]} .. {ids[-1]}')
print(f'distinct datatypes: {sorted(set(r[\"datatype\"] for r in rows))}')
print(f'distinct elm_constructs: {sorted(set(r[\"elm_construct\"] for r in rows))}')
print(f'distinct source_modules: {len(set(r[\"source_module\"] for r in rows))} files')
"
```

Expected:
- `rows` matches the count in the methodology doc's *Inventory pass metadata*
- `columns` is `['id', 'name', 'eheza_field_path', 'source_module', 'elm_construct', 'datatype']`
- `first/last id` is `EHEZA-0001 .. EHEZA-NNNN` matching row count
- `distinct datatypes` is a subset of `['Boolean', 'Coded', 'Date', 'N/A', 'Numeric', 'Text']`
- `distinct elm_constructs` is a subset of `['record_field', 'record_type', 'union_constructor', 'union_type']`
- `distinct source_modules` is between 30 and 37 (some `Backend/*` modules may have no concept-bearing types)

- [ ] **Step 4: Confirm existing per-encounter CSVs are unchanged**

```bash
cd /var/www/html/ihangane && git diff docs/ocl-pih-mappings -- docs/ocl/*-concepts.csv docs/ocl/*-mappings.csv docs/ocl/translations.jsonl
```

Expected: no output (no changes to those files between `docs/ocl-pih-mappings` and `docs/eheza-concepts-master`). The master pass MUST NOT have edited any per-encounter file.

- [ ] **Step 5: Confirm the OCL converter still works with the unchanged per-encounter files**

```bash
cd /var/www/html/ihangane && python3 docs/ocl/csv_to_ocl_json.py | python3 -c "
import sys, json
n = 0
for line in sys.stdin:
    json.loads(line)
    n += 1
print(f'parsed {n} json-lines, all valid')
"
```

Expected: `parsed 434 json-lines, all valid` (217 concepts + 217 mappings — same as the patient-registration baseline). The master CSV is intentionally NOT picked up by `csv_to_ocl_json.py`'s glob (the converter looks for `*-concepts.csv` / `*-mappings.csv`; the master file ends `-master.csv` so it's outside that glob).

- [ ] **Step 6: Print final summary**

Tell the user:
*"E-Heza concepts master inventory pass complete. NNNN rows in `docs/ocl/eheza-concepts-master.csv`, methodology doc + README section in place. Branch `docs/eheza-concepts-master` has 5 commits ahead of `docs/ocl-pih-mappings`; not pushed (push when ready). Existing per-encounter PIH-mapped CSVs and the OCL release `v1.1-add-patient-registration` are untouched. Master is local-only — when the first cross-org mapping pass starts (UVL-Burundi or otherwise), the master serves as the search baseline."*

No commit (verification only).
