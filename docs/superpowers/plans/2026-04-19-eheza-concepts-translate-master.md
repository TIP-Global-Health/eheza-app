# E-Heza concepts translate master — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Produce one CSV (`docs/ocl/eheza-concepts-translate.csv`) cataloguing every user-facing label in `client/src/elm/Translate.elm` that survives a refined heuristic filter — including derived clinical concepts (Gravida, BMI, ApgarScore, etc.) that the structural master can't see because they exist only as UI labels, not as Elm record fields or union constructors.

**Architecture:** Pure documentation deliverable — three new files (master CSV, methodology doc, README section) plus this spec/plan committed once at the end. No code, no schema changes, no edits to existing per-encounter PIH-mapped CSVs or to the structural master CSV. Build process uses a throwaway Python walker (`/tmp/eheza-translate-walker.py`, never committed) that parses the `type TranslationId` block, applies the heuristic ruleset, and for surviving union-arg constructors walks into the inline `case option of` dispatch to extract per-value translations.

**Tech Stack:** CSV (RFC 4180), Python 3 for the scratch walker (regex on Elm source — no formal Elm parser required for the depth this walk needs), Markdown.

**Spec:** `docs/superpowers/specs/2026-04-19-eheza-concepts-translate-master-design.md`

**Conventions enforced throughout:**
- Per global preference: **always ask the user before running `git commit` or `git push`** — and per the user's explicit instruction for this work, **only ONE commit gate at the very end** (single combined commit covering spec + plan + methodology + CSV + README).
- Per CLAUDE.md: the single commit message ends with `[ci skip]` (after the Co-Authored-By line).
- Per spec §Schema: 6 columns (`id`, `translation_id`, `english`, `kinyarwanda`, `kirundi`, `somali`). No more, no fewer. RFC 4180 quoting; UTF-8.
- Per spec §Walk methodology: walk order is `type TranslationId` declaration in source order. ID assignment is sequential over emitted rows (header rows AND leaf rows count). **IDs immutable after the single commit.**
- Per spec §Build process: walker lives at `/tmp/eheza-translate-walker.py` and is NOT committed.

---

## File Structure

**Create (committed in the single end-of-process commit):**
- `docs/ocl/eheza-concepts-translate.csv` — labels inventory, ~2000–4000 rows.
- `docs/ocl/eheza-concepts-translate.md` — methodology doc.

**Modify (committed in the single end-of-process commit):**
- `docs/ocl/README.md` — new section after the existing *Master inventory — `eheza-concepts-master.csv`* section.

**Already done before this plan starts:**
- Branch `docs/eheza-concepts-master` is the active branch (no new branch).
- The structural master + its methodology + spec/plan are all committed (commits `715e46d27`, `5c60de0f7`, `445405820`, `f0765c429`, plus the patient-registration commits below those).

**Will be committed in the single end-of-process commit too:**
- `docs/superpowers/specs/2026-04-19-eheza-concepts-translate-master-design.md` (already written, uncommitted)
- `docs/superpowers/plans/2026-04-19-eheza-concepts-translate-master.md` (this file, uncommitted)

**Working / scratch files (uncommitted, in `/tmp/`):**
- `/tmp/eheza-translate-walker.py` — Python walker.
- `/tmp/eheza-translate-preliminary.csv` — first walker output before tuning.
- `/tmp/eheza-translate-walk-summary.txt` — per-constructor kept/dropped log.
- `/tmp/eheza-translate-master.csv` — final hand-verified file before being copied into `docs/ocl/`.

---

## Task 1: Write the methodology doc

**Files:**
- Create: `/var/www/html/ihangane/docs/ocl/eheza-concepts-translate.md`

This doc gets created BEFORE the CSV so reviewers of the eventual single commit see the rules the data was built against.

- [ ] **Step 1: Read voice/structure templates**

Read `docs/ocl/eheza-concepts-master.md` end-to-end (the structural master's methodology doc, committed at `5c60de0f7`). It's the closest template — same audience, same general shape. Match the voice (compact, factual, code-fences, tables) but write FRESH content per the spec.

- [ ] **Step 2: Write the file with these sections, in this order**

Create `docs/ocl/eheza-concepts-translate.md` with:

**1. Header paragraph** (no heading, 4-5 sentences):
- This file documents `docs/ocl/eheza-concepts-translate.csv`
- The labels master is built from `client/src/elm/Translate.elm` (the `type TranslationId` block, lines 329–2192 of source)
- Purpose: catalogue user-facing labels including derived clinical concepts (Gravida, BMI, ApgarScore, etc.) that the structural master can't surface
- Has its own `EHEZA-T-NNNN` ID space; intentionally separate from the structural master and from per-encounter PIH-mapped CSVs (no cross-references)
- Future cross-organisation mapping passes (UVL-Burundi being a known upcoming consumer) consume this master alongside (or instead of, or combined with) the structural master — that decision is deferred to a separate brainstorm

**2. ## Schema** — reproduce the 6-column table from spec §Schema verbatim, plus the bullet about how header / leaf / zero-arg rows are distinguished without a dedicated column. Include the AbdomenCPESign concrete example table.

**3. ## Walk methodology** — reproduce spec §Walk methodology in full. Include sub-headings for: ### Walk order, ### Per-constructor emit decision, ### Heuristic filter rules (with sub-bullets for Drop by suffix / Drop by prefix / Drop by exact name / Drop UI-only union types / KEEP exceptions), ### ID assignment, ### Edge cases.

**4. ## Inventory pass metadata** — placeholder bullets to be filled in at Task 5:
```markdown
- **Walk date**: `<YYYY-MM-DD>`
- **Source tree SHA**: `<git rev-parse HEAD output>`
- **Walker tool**: scratch Python script at `/tmp/eheza-translate-walker.py` (not committed; see *Build process* below)
- **Initial row count**: `<filled in at CSV finalize step>`
- **Heuristic ruleset version**: `<initial OR tuned-from-initial — capture any rules added during build>`
```

**5. ## Process discipline for future Elm changes** — one short paragraph:

> If you add a new `TranslationId` constructor to `client/src/elm/Translate.elm` and it survives the heuristic ruleset documented above, append a row to `eheza-concepts-translate.csv` with the next available `EHEZA-T-NNNN` id as part of the same PR. If the new constructor takes a union arg (with the same conditions: union appears in the structural master), also append leaf rows for each case branch in the inline `case option of` dispatch. The walk order (source-order in the `type TranslationId` block) is only used to assign IDs during the *initial* inventory pass; net-new entries after that get the next free id regardless of where they fall in source order.

**6. ## Scope** — two paragraphs:

- **In scope** — the `type TranslationId` declaration block in `client/src/elm/Translate.elm`. 1860 top-level constructors before filtering; ~600–1000 expected to survive the heuristic filter; ~2000–4000 total rows once leaves of surviving union-arg constructors are expanded.
- **Out of scope** — sub-translator interiors beyond the immediate case branch level (nested dispatches not followed); editing of translation strings (Translate.elm is the source of truth, copied verbatim); `concept_class`/`description`/`confidence` columns (this is an inventory, not a curated dictionary); upload to OCL.

**7. ## Explicit non-relationship to other docs/ocl/ files** — short paragraph:
- Translate master, structural master, and per-encounter PIH-mapped CSVs all have separate ID spaces and separate purposes
- The translate master is NOT updated when the structural master gains rows, and vice versa
- The translate master is NOT updated when per-encounter PIH mappings change
- Future cross-org mapping passes choose which master(s) to consume based on the use case; conflating them would force every label change to ripple into the structural master and vice versa, which is the maintenance pattern this separation avoids

- [ ] **Step 3: Render-check the file**

Run:
```bash
python3 -c "
import re, pathlib
text = pathlib.Path('/var/www/html/ihangane/docs/ocl/eheza-concepts-translate.md').read_text()
print('headings (in order):')
for h in re.findall(r'^##+ .*', text, re.M):
    print(' ', h)
print('eh-id-style citations (must be 0):', len(re.findall(r'\bEH-[A-Z]+-\d+\b', text)))
print('eheza-t-NNNN citation count (>=2 expected — schema examples):', len(re.findall(r'EHEZA-T-\d{4}', text)))
print('placeholder strings present:', sorted(set(re.findall(r'<[^>]+>', text))))
print('total lines:', len(text.splitlines()))
"
```

Expected:
- 7 `##` headings in order: `Schema`, `Walk methodology`, `Inventory pass metadata`, `Process discipline for future Elm changes`, `Scope`, `Explicit non-relationship to other docs/ocl/ files`. (Plus `### …` sub-headings under Walk methodology.)
- `eh-id-style citations` is 0 — must NOT cite per-encounter PIH ids; this doc is independent.
- `eheza-t-NNNN citation count` ≥ 2 (schema examples).
- `placeholder strings present` includes `<YYYY-MM-DD>`, `<git rev-parse HEAD output>`, `<filled in at CSV finalize step>`, `<initial OR tuned-from-initial — capture any rules added during build>`.
- `total lines` in range 150–250.

NO COMMIT. Move directly to Task 2.

---

## Task 2: Write the scratch walker

**Files:**
- Create: `/tmp/eheza-translate-walker.py` (NOT committed)

- [ ] **Step 1: Save the walker script verbatim**

Save to `/tmp/eheza-translate-walker.py`:

```python
#!/usr/bin/env python3
"""Walk Translate.elm's TranslationId block and emit labels-master CSV.

Output to stdout: CSV with header `id,translation_id,english,kinyarwanda,kirundi,somali`.
Per-constructor summary to stderr.

Usage:
    python3 /tmp/eheza-translate-walker.py > /tmp/eheza-translate-preliminary.csv 2> /tmp/eheza-translate-walk-summary.txt
"""
import csv
import os
import re
import sys

REPO = "/var/www/html/ihangane"
TRANSLATE_PATH = os.path.join(REPO, "client/src/elm/Translate.elm")
STRUCTURAL_MASTER = os.path.join(REPO, "docs/ocl/eheza-concepts-master.csv")

# === Heuristic ruleset (Section §Walk methodology of the spec) ===
DROP_SUFFIXES = ("Title", "Helper", "Help", "Page", "Button", "Tab")
DROP_PREFIXES = ("Add", "Click", "Save", "Submit", "Cancel", "Edit", "Delete",
                 "Loading", "Show", "Hide", "Remove")
DROP_EXACT = {"Accept", "Actions", "Activities", "ActionsTaken", "ActionsToTake",
              "Yes", "No", "OK", "Done", "Continue", "Back", "Next", "Previous",
              "Close", "Open", "Send", "Receive", "Loading", "Saving", "Error", "Success"}
KEEP_SUFFIXES = ("Label", "Question", "Warning")  # override DROP_SUFFIXES


def load_in_scope_unions():
    """Read structural master, return set of union_type field paths."""
    unions = set()
    with open(STRUCTURAL_MASTER) as f:
        for row in csv.DictReader(f):
            if row.get("elm_construct") == "union_type":
                unions.add(row["eheza_field_path"])
    return unions


def strip_comments(src):
    src = re.sub(r"\{-[\s\S]*?-\}", "", src)
    src = re.sub(r"--[^\n]*", "", src)
    return src


def prettify(name):
    """Mechanical CamelCase -> 'First word lowercased rest'."""
    s = re.sub(r"(?<=[a-z0-9])(?=[A-Z])", " ", name)
    s = re.sub(r"(?<=[A-Z])(?=[A-Z][a-z])", " ", s)
    parts = s.split()
    if not parts:
        return name
    return parts[0][0].upper() + parts[0][1:].lower() + (
        " " + " ".join(p.lower() for p in parts[1:]) if len(parts) > 1 else "")


def should_keep(ctor_name, arg_type, in_scope_unions):
    """Apply the heuristic ruleset. Returns (keep: bool, reason: str)."""
    # KEEP exceptions take precedence over DROP suffixes
    for suf in KEEP_SUFFIXES:
        if ctor_name.endswith(suf):
            # Still drop if union-arg with UI-only union type
            if arg_type and not arg_type[0].islower() and arg_type not in ("Int",) and arg_type not in in_scope_unions:
                return False, f"DROP: union-arg wrapping UI-only type {arg_type}"
            return True, f"KEEP: {suf} suffix exception"
    for suf in DROP_SUFFIXES:
        if ctor_name.endswith(suf):
            return False, f"DROP: suffix {suf}"
    for pre in DROP_PREFIXES:
        if ctor_name.startswith(pre):
            return False, f"DROP: prefix {pre}"
    if ctor_name in DROP_EXACT:
        return False, "DROP: exact-name UI atom"
    # Union-arg with UI-only union type
    if arg_type and not arg_type[0].islower() and arg_type not in ("Int",):
        if arg_type not in in_scope_unions:
            return False, f"DROP: union-arg wrapping UI-only type {arg_type}"
    return True, "KEEP"


def parse_translation_id_block(src):
    """Extract list of (ctor_name, arg_type_or_None, source_line_number) in source order."""
    m = re.search(r"^type TranslationId\s*\n", src, re.MULTILINE)
    if not m:
        raise SystemExit("Could not locate `type TranslationId` block")
    block_start = m.end()
    # Find the end of the block (next top-level lowercase declaration)
    end_match = re.search(r"^[a-z]\w*\s*[:=]", src[block_start:], re.MULTILINE)
    block_end = block_start + (end_match.start() if end_match else len(src) - block_start)
    block = src[block_start:block_end]

    constructors = []
    for line in block.split("\n"):
        m = re.match(r"^\s*[|=]\s*([A-Z]\w*)(?:\s+(.+?))?\s*$", line)
        if m:
            ctor_name = m.group(1)
            rest = (m.group(2) or "").strip()
            # First whitespace-separated token of rest is the arg type
            arg_type = None
            if rest:
                arg_type = rest.split()[0].rstrip(",")
                # Strip leading '(' for special-arg constructors
                if arg_type.startswith("("):
                    arg_type = arg_type
            constructors.append((ctor_name, arg_type))
    return constructors


def find_inline_dispatch(src, ctor_name):
    """Find the `<ctor_name> option ->\\n case option of` block and return its body text."""
    # Look for the pattern: ctor_name option ->\n            case option of\n                ...
    # Or: ctor_name <varname> -> ... case <varname> of ...
    pattern = re.compile(
        r"^\s+" + re.escape(ctor_name) + r"\s+(\w+)\s*->\s*\n\s+case\s+\1\s+of\s*\n",
        re.MULTILINE,
    )
    m = pattern.search(src)
    if not m:
        return None
    start = m.end()
    # Find the end of this `case of` block — the next case branch dedented to same level
    # as the outer ctor branch. Heuristic: find the next top-level "<Name> [arg] ->" pattern at outer indent.
    # Outer indent is whatever indentation `ctor_name` had on its line.
    outer_indent_match = re.match(r"^(\s+)" + re.escape(ctor_name), src[m.start():m.end()])
    outer_indent = outer_indent_match.group(1) if outer_indent_match else "        "
    # Look for next line with same indent + capital letter (= next case branch sibling)
    end_pattern = re.compile(r"^" + outer_indent + r"[A-Z]\w*", re.MULTILINE)
    end_m = end_pattern.search(src, start)
    end = end_m.start() if end_m else len(src)
    return src[start:end]


def parse_case_branches(body):
    """Parse a `case option of` body. Yield (value_name, en, rw, rn, so) per branch."""
    # Each branch: `<ValueName> ->\n    { english = "...", kinyarwanda = Just "...", ... }`
    branches = re.split(r"\n(?=\s+[A-Z]\w*\s*->)", body)
    for branch in branches:
        m = re.match(r"\s+([A-Z]\w*)\s*->\s*\n([\s\S]*)", branch)
        if not m:
            continue
        value_name = m.group(1)
        body_text = m.group(2)
        # Extract translations
        en = extract_field(body_text, "english")
        rw = extract_field(body_text, "kinyarwanda")
        rn = extract_field(body_text, "kirundi")
        so = extract_field(body_text, "somali")
        if en is None:
            # branch may dispatch to nested call — skip with warning
            print(f"  WARN: case branch {value_name} has no inline english", file=sys.stderr)
            continue
        yield value_name, en, rw, rn, so


def extract_field(text, name):
    """Extract a field value from a TranslationSet record body. Returns None if not present, '' if empty, or the literal."""
    # Match: name = "..."  OR  name = Just "..."  OR  name = Nothing
    m = re.search(r"\b" + re.escape(name) + r"\s*=\s*(Just\s+\"((?:\\.|[^\"\\])*)\"|\"((?:\\.|[^\"\\])*)\"|Nothing)", text)
    if not m:
        return None
    if m.group(2) is not None:
        return m.group(2)
    if m.group(3) is not None:
        return m.group(3)
    return None  # Nothing


def parse_zero_arg_translation(src, ctor_name):
    """For zero-arg TranslationIds, find the `<ctor_name> ->\\n { english = "..." ... }` block."""
    pattern = re.compile(
        r"^\s+" + re.escape(ctor_name) + r"\s*->\s*\n([\s\S]*?)\n\s+[A-Z]\w*\s*[->\s]",
        re.MULTILINE,
    )
    m = pattern.search(src)
    if not m:
        return None, None, None, None
    body = m.group(1)
    en = extract_field(body, "english")
    rw = extract_field(body, "kinyarwanda")
    rn = extract_field(body, "kirundi")
    so = extract_field(body, "somali")
    return en, rw, rn, so


def main():
    src = open(TRANSLATE_PATH).read()
    src_no_comments = strip_comments(src)
    in_scope_unions = load_in_scope_unions()
    print(f"in-scope unions from structural master: {len(in_scope_unions)}", file=sys.stderr)

    constructors = parse_translation_id_block(src_no_comments)
    print(f"top-level TranslationId constructors: {len(constructors)}", file=sys.stderr)

    writer = csv.writer(sys.stdout)
    writer.writerow(["id", "translation_id", "english", "kinyarwanda", "kirundi", "somali"])

    next_id = 1
    kept = 0
    dropped = 0
    leaves_emitted = 0
    for ctor_name, arg_type in constructors:
        keep, reason = should_keep(ctor_name, arg_type, in_scope_unions)
        if not keep:
            dropped += 1
            print(f"  DROP {ctor_name}: {reason}", file=sys.stderr)
            continue
        kept += 1

        if arg_type is None:
            # Zero-arg label
            en, rw, rn, so = parse_zero_arg_translation(src_no_comments, ctor_name)
            if en is None:
                en = prettify(ctor_name)
                print(f"  WARN: zero-arg {ctor_name} had no inline english; using prettified name", file=sys.stderr)
            writer.writerow([f"EHEZA-T-{next_id:04d}", ctor_name, en or "", rw or "", rn or "", so or ""])
            next_id += 1
        elif arg_type and not arg_type[0].islower() and arg_type != "Int" and arg_type in in_scope_unions:
            # Union-arg with in-scope union — emit header + leaves
            writer.writerow([f"EHEZA-T-{next_id:04d}", ctor_name, prettify(ctor_name), "", "", ""])
            next_id += 1
            dispatch = find_inline_dispatch(src_no_comments, ctor_name)
            if dispatch is None:
                print(f"  WARN: union-arg {ctor_name} has no inline `case of` block", file=sys.stderr)
                continue
            for value_name, en, rw, rn, so in parse_case_branches(dispatch):
                writer.writerow([
                    f"EHEZA-T-{next_id:04d}",
                    f"{arg_type}.{value_name}",
                    en, rw or "", rn or "", so or "",
                ])
                next_id += 1
                leaves_emitted += 1
        else:
            # Special-arg (Int, complex types) — single header row, no leaves
            writer.writerow([f"EHEZA-T-{next_id:04d}", ctor_name, prettify(ctor_name), "", "", ""])
            next_id += 1

    print(f"\nSUMMARY: kept={kept} dropped={dropped} total_rows={next_id - 1} (incl. {leaves_emitted} leaves)", file=sys.stderr)


if __name__ == "__main__":
    main()
```

- [ ] **Step 2: Verify the script parses as Python**

```bash
python3 -m py_compile /tmp/eheza-translate-walker.py && echo "ok"
```

Expected: `ok`. Any SyntaxError = typo when copying — fix before proceeding.

NO COMMIT. Move to Task 3.

---

## Task 3: Run the walker, sanity-check, iterate until output looks right

**Files:**
- Write: `/tmp/eheza-translate-preliminary.csv` (scratch)
- Write: `/tmp/eheza-translate-walk-summary.txt` (scratch)
- Modify: `/tmp/eheza-translate-walker.py` (when iterating to fix bugs)

- [ ] **Step 1: Run the walker**

```bash
cd /var/www/html/ihangane && python3 /tmp/eheza-translate-walker.py \
  > /tmp/eheza-translate-preliminary.csv 2> /tmp/eheza-translate-walk-summary.txt
echo "exit=$?"
```

Expected: `exit=0`. Any non-zero = walker crashed; read stderr to find the line that failed (usually a regex edge case in some unusual TranslationId), fix the walker, re-run.

- [ ] **Step 2: Inspect the summary tail**

```bash
tail -20 /tmp/eheza-translate-walk-summary.txt
echo "---"
echo "row count in preliminary: $(($(wc -l < /tmp/eheza-translate-preliminary.csv) - 1))"
```

Expected output ends with something like:
```
SUMMARY: kept=NNN dropped=NNN total_rows=NNNN (incl. NNNN leaves)
```

The total row count should be in range 2000–4000 per spec. If wildly off:
- Way too high → heuristic isn't dropping enough; check the dropped counts and log
- Way too low → walker can't find the inline case-of dispatches (the `find_inline_dispatch` regex doesn't match the actual Translate.elm formatting)

- [ ] **Step 3: Sample 10 random rows to verify shape and content**

```bash
shuf -n 10 /tmp/eheza-translate-preliminary.csv | column -t -s, -W 3
```

For each row, verify:
- `id` is `EHEZA-T-NNNN` zero-padded
- `translation_id` is either bare or qualified `<Type>.<Value>`
- `english` is non-empty
- locale columns are either populated or `null`-equivalent (empty string is OK; the spec says `null` but CSV represents it as empty)

If anything looks wrong, fix the walker and re-run.

- [ ] **Step 4: Spot-check that derived clinical concepts ARE in the output**

```bash
echo "Gravida row(s):"
grep -E "^EHEZA-T-[0-9]+,Gravida," /tmp/eheza-translate-preliminary.csv || echo "(MISSING — Gravida should be a zero-arg label)"
echo ""
echo "AbdomenCPESign rows (header + leaves):"
grep -E "^EHEZA-T-[0-9]+,AbdomenCPESign(\.|,)" /tmp/eheza-translate-preliminary.csv | head -10
echo ""
echo "BMI row(s):"
grep -E "^EHEZA-T-[0-9]+,BMI" /tmp/eheza-translate-preliminary.csv || echo "(check Translate.elm — might be named differently)"
```

Expected:
- Gravida row present with translations
- AbdomenCPESign header + 7 leaf rows present
- BMI row (if it exists in Translate.elm as a TranslationId)

If Gravida/AbdomenCPESign are missing, the walker has a parsing bug — fix.

- [ ] **Step 5: Spot-check that obvious UI noise was DROPPED**

```bash
for ui_term in Save SaveLabAndDrugStock Cancel ClickHere AddChild DashboardActivePage AcuteIllnessActivityTitle; do
  hit=$(grep -E "^EHEZA-T-[0-9]+,$ui_term," /tmp/eheza-translate-preliminary.csv 2>/dev/null)
  if [ -n "$hit" ]; then
    echo "  LEAKED: $ui_term"
  else
    echo "  dropped (good): $ui_term"
  fi
done
```

Expected: all should be dropped. Any LEAKED hit = heuristic missed a pattern; refine the rule (add to DROP_PREFIXES/SUFFIXES/EXACT in the walker), re-run.

- [ ] **Step 6: Iterate until happy**

If steps 3–5 surfaced bugs:
- Edit `/tmp/eheza-translate-walker.py` to fix
- Re-run step 1
- Re-run steps 3–5

If you tuned the heuristic ruleset (added drop patterns, added KEEP exceptions, added inline-dispatch pattern variations), **note the changes for the methodology doc update in Task 5**.

NO COMMIT. Move to Task 4.

---

## Task 4: Hand-verification + final master file in `/tmp/`

**Files:**
- Read: `/tmp/eheza-translate-preliminary.csv`
- Write: `/tmp/eheza-translate-master.csv`

- [ ] **Step 1: Copy preliminary to working file**

```bash
cp /tmp/eheza-translate-preliminary.csv /tmp/eheza-translate-master.csv
```

- [ ] **Step 2: Apply acronym fix-ups in bulk** (same approach as the structural master pass)

Some constructor-name prettifications produce awkward output (e.g., `HIVStatusLabel` → `Hiv status label`, where it should be `HIV status label`). Fix in bulk:

```bash
python3 <<'EOF'
import csv
ACRONYMS = {
    "Hiv": "HIV", "Muac": "MUAC", "Bmi": "BMI", "Anc": "ANC", "Bp": "BP",
    "Hct": "HCT", "Hmis": "HMIS", "Gps": "GPS", "Ncd": "NCD", "Tb": "TB",
    "Rdt": "RDT", "Iud": "IUD", "Opv": "OPV", "Bcg": "BCG", "Dpt": "DPT",
    "Ipv": "IPV", "Mr": "MR", "Pcv": "PCV", "Ncda": "NCDA", "Chw": "CHW",
    "Fbf": "FBF", "Fbfs": "FBFs", "Hc": "HC", "Edd": "EDD", "Rh": "RH",
    "Hb": "HB", "Hba1c": "HbA1c", "Dot": "DOT", "Asap": "ASAP",
    "Ast": "AST", "Alt": "ALT", "Bun": "BUN", "Ecd": "ECD", "Hdl": "HDL",
    "Ldl": "LDL", "Hpv": "HPV", "Lmp": "LMP", "Mms": "MMS", "Ors": "ORS",
    "Rutf": "RUTF", "Cpe": "CPE",
}
out_rows = []
with open("/tmp/eheza-translate-master.csv") as f:
    reader = csv.DictReader(f)
    fieldnames = reader.fieldnames
    for r in reader:
        n = r["english"]
        for wrong, right in ACRONYMS.items():
            n = n.replace(f" {wrong} ", f" {right} ")
            if n.startswith(f"{wrong} "):
                n = right + n[len(wrong):]
            if n.endswith(f" {wrong}"):
                n = n[:-len(wrong)] + right
            if n == wrong:
                n = right
        r["english"] = n
        out_rows.append(r)
with open("/tmp/eheza-translate-master.csv", "w", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=fieldnames)
    writer.writeheader()
    writer.writerows(out_rows)
print(f"applied acronym fix-ups across {len(out_rows)} rows")
EOF
```

Expected: prints the row count.

- [ ] **Step 3: Spot-check 20 random rows post-acronym fix-ups**

```bash
shuf -n 20 /tmp/eheza-translate-master.csv | column -t -s, -W 3
```

If anything reads badly (other acronyms missed), add to dictionary in step 2 and re-run. The helper is idempotent.

- [ ] **Step 4: Verify the master CSV count matches preliminary** (we only edited `english`, not row count)

```bash
echo "preliminary rows: $(($(wc -l < /tmp/eheza-translate-preliminary.csv) - 1))"
echo "master rows:      $(($(wc -l < /tmp/eheza-translate-master.csv) - 1))"
```

Expected: identical. Any diff = step 2 dropped/added rows; investigate.

NO COMMIT. Move to Task 5.

---

## Task 5: Run validation suite + final acceptance

**Files:**
- Read: `/tmp/eheza-translate-master.csv`

All 6 checks from spec §Validation. Plus the cross-master sanity check.

- [ ] **Step 1: No duplicate `id`**

```bash
dups=$(awk -F, 'NR>1 {print $1}' /tmp/eheza-translate-master.csv | sort | uniq -d | wc -l)
echo "duplicate id count: $dups (must be 0)"
```

Expected: 0.

- [ ] **Step 2: IDs contiguous from `EHEZA-T-0001`**

```bash
python3 -c "
import csv
with open('/tmp/eheza-translate-master.csv') as f:
    ids = [r['id'] for r in csv.DictReader(f)]
nums = [int(i.split('-')[2]) for i in ids]
expected = list(range(1, len(nums) + 1))
print(f'rows={len(nums)}, first={ids[0]}, last={ids[-1]}')
print(f'contiguous={nums == expected}')
print(f'first gap={next((i for i, (a, b) in enumerate(zip(nums, expected)) if a != b), \"none\")}')
"
```

Expected: `contiguous=True`, `first gap=none`.

- [ ] **Step 3: Every header `translation_id` (non-dotted) exists in `Translate.elm`**

```bash
python3 <<'EOF'
import csv
src = open('/var/www/html/ihangane/client/src/elm/Translate.elm').read()
unresolved = []
with open('/tmp/eheza-translate-master.csv') as f:
    for r in csv.DictReader(f):
        tid = r['translation_id']
        if '.' in tid:
            continue
        # Look for the ctor in the type TranslationId block
        import re
        if not re.search(r"^\s*[|=]\s*" + re.escape(tid) + r"(\s|$)", src, re.MULTILINE):
            unresolved.append((r['id'], tid))
print(f"unresolved header translation_ids: {len(unresolved)}")
for u in unresolved[:10]:
    print(' ', u)
EOF
```

Expected: 0 unresolved. Any unresolved = walker emitted a name that's not actually a TranslationId; fix walker and re-run from Task 3.

- [ ] **Step 4: Every leaf `translation_id` (dotted) corresponds to a real case branch in `Translate.elm`**

```bash
python3 <<'EOF'
import csv, re
src = open('/var/www/html/ihangane/client/src/elm/Translate.elm').read()
unresolved = []
with open('/tmp/eheza-translate-master.csv') as f:
    for r in csv.DictReader(f):
        tid = r['translation_id']
        if '.' not in tid:
            continue
        union_type, value = tid.split('.', 1)
        # Look for "<value> ->" inside the file (case branches)
        # This is loose — just confirm <value> appears as a case branch somewhere
        if not re.search(r"^\s+" + re.escape(value) + r"\s*->", src, re.MULTILINE):
            unresolved.append((r['id'], tid))
print(f"unresolved leaf translation_ids: {len(unresolved)}")
for u in unresolved[:10]:
    print(' ', u)
EOF
```

Expected: 0 unresolved. Any unresolved = walker mis-extracted a value name; fix and re-run from Task 3.

- [ ] **Step 5: Every row has non-empty `english`**

```bash
empty=$(awk -F, 'NR>1 && $3=="" {n++} END {print n+0}' /tmp/eheza-translate-master.csv)
echo "empty english count: $empty (must be 0)"
```

Expected: 0.

- [ ] **Step 6: Row count sanity**

```bash
echo "row count: $(($(wc -l < /tmp/eheza-translate-master.csv) - 1))"
```

Expected: in range 2000–4000.

- [ ] **Step 7: Cross-master sanity check (advisory)**

```bash
python3 <<'EOF'
import csv
master_paths = set()
with open('/var/www/html/ihangane/docs/ocl/eheza-concepts-master.csv') as f:
    for r in csv.DictReader(f):
        master_paths.add(r['eheza_field_path'])
matched = unmatched = 0
with open('/tmp/eheza-translate-master.csv') as f:
    for r in csv.DictReader(f):
        tid = r['translation_id']
        if '.' in tid:
            if tid in master_paths:
                matched += 1
            else:
                unmatched += 1
print(f"leaves matching structural master: {matched}")
print(f"leaves NOT matching structural master: {unmatched}")
print(f"  (high unmatched = UI-only union types snuck past the filter; consider tuning rules)")
EOF
```

Expected: matched count is significant (most leaves should be union constructors already in the structural master); unmatched count should be modest. If unmatched is huge (>500), the heuristic is letting too many UI unions through — go back to Task 3, tune `should_keep`'s UI-only-union check, re-run.

NO COMMIT. Move to Task 6.

---

## Task 6: Move CSV into repo + fill methodology doc metadata

**Files:**
- Create (in repo): `/var/www/html/ihangane/docs/ocl/eheza-concepts-translate.csv`
- Modify: `/var/www/html/ihangane/docs/ocl/eheza-concepts-translate.md`

- [ ] **Step 1: Copy validated CSV into repo**

```bash
cp /tmp/eheza-translate-master.csv /var/www/html/ihangane/docs/ocl/eheza-concepts-translate.csv
echo "row count in repo file: $(($(wc -l < /var/www/html/ihangane/docs/ocl/eheza-concepts-translate.csv) - 1))"
```

Expected: same count as Task 5 step 6.

- [ ] **Step 2: Capture metadata for methodology doc**

```bash
SHA=$(git rev-parse HEAD)
DATE=$(date -u +%Y-%m-%d)
ROWS=$(($(wc -l < /var/www/html/ihangane/docs/ocl/eheza-concepts-translate.csv) - 1))
echo "metadata to substitute:"
echo "  walk date:  $DATE"
echo "  source SHA: $SHA"
echo "  row count:  $ROWS"
```

- [ ] **Step 3: Update the methodology doc's *Inventory pass metadata* section**

Edit `/var/www/html/ihangane/docs/ocl/eheza-concepts-translate.md` to replace the placeholder block:

```markdown
- **Walk date**: `<YYYY-MM-DD>`
- **Source tree SHA**: `<git rev-parse HEAD output>`
- **Walker tool**: scratch Python script at `/tmp/eheza-translate-walker.py` (not committed; see *Build process* below)
- **Initial row count**: `<filled in at CSV finalize step>`
- **Heuristic ruleset version**: `<initial OR tuned-from-initial — capture any rules added during build>`
```

with the actual values. If any heuristic rules were added/changed during Task 3 step 6 iterations, document the additions explicitly here (e.g., "Added `*Tab` to DROP_SUFFIXES because Translate.elm has a few `*Tab` constructors not anticipated in the spec.").

- [ ] **Step 4: Confirm both files are present and correct**

```bash
git status -- docs/ocl/eheza-concepts-translate.csv docs/ocl/eheza-concepts-translate.md
```

Expected: CSV is untracked (new), methodology doc is modified (was created in Task 1, modified in step 3 above — but since Task 1 didn't commit, both files are still untracked from git's view).

NO COMMIT. Move to Task 7.

---

## Task 7: README update

**Files:**
- Modify: `/var/www/html/ihangane/docs/ocl/README.md`

- [ ] **Step 1: Locate insertion point**

```bash
grep -n "Master inventory\|Target OCL source" docs/ocl/README.md
```

Expected: shows the existing `## Master inventory — \`eheza-concepts-master.csv\`` heading and the `## Target OCL source` heading. The new section goes between them.

- [ ] **Step 2: Insert the new section**

Use the Edit tool to add this block immediately before the `## Target OCL source` heading:

```markdown
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
```

- [ ] **Step 3: Verify the edit landed**

```bash
grep -nE "Labels inventory|eheza-concepts-translate" docs/ocl/README.md
```

Expected: at least 3 hits — the heading and the two filename mentions in the new section. Confirm by reading the file around the insertion point that the section sits between `## Master inventory` and `## Target OCL source`.

NO COMMIT. Move to Task 8.

---

## Task 8: Final verification + single end-of-process commit

**Files:** none modified; verification only, then the commit.

- [ ] **Step 1: Confirm working tree state**

```bash
git status -- docs/ocl/ docs/superpowers/
```

Expected: 4 files in scope:
- `docs/ocl/eheza-concepts-translate.csv` — untracked (new)
- `docs/ocl/eheza-concepts-translate.md` — untracked (new)
- `docs/ocl/README.md` — modified
- `docs/superpowers/specs/2026-04-19-eheza-concepts-translate-master-design.md` — untracked (new, written during brainstorming)
- `docs/superpowers/plans/2026-04-19-eheza-concepts-translate-master.md` — untracked (new, this file)

(5 files total in the upcoming commit.)

- [ ] **Step 2: Confirm structural master + per-encounter PIH CSVs are unchanged vs `docs/ocl-pih-mappings`**

```bash
diff_count=$(git diff f0765c429 -- docs/ocl/*-concepts.csv docs/ocl/*-mappings.csv docs/ocl/translations.jsonl docs/ocl/eheza-concepts-master.csv docs/ocl/eheza-concepts-master.md 2>&1 | wc -l)
echo "diff line count vs structural-master commit: $diff_count (should be 0)"
```

Expected: 0. Any diff = something inadvertently modified an existing file; revert.

- [ ] **Step 3: Confirm OCL converter still emits 434 valid json-lines** (i.e., the new CSV doesn't get accidentally picked up by `csv_to_ocl_json.py`'s glob)

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

Expected: `parsed 434 json-lines, all valid`. The new file ends `-translate.csv` so it's outside the `*-concepts.csv` / `*-mappings.csv` glob — confirms the converter sees only the per-encounter PIH CSVs and emits the same count as before.

- [ ] **Step 4: Re-validate the labels master at its committed path**

```bash
cd /var/www/html/ihangane && python3 -c "
import csv
with open('docs/ocl/eheza-concepts-translate.csv') as f:
    rows = list(csv.DictReader(f))
ids = [r['id'] for r in rows]
print(f'rows={len(rows)}')
print(f'columns={list(rows[0].keys())}')
print(f'first/last id: {ids[0]} .. {ids[-1]}')
print(f'header rows (no dot): {sum(1 for r in rows if \".\" not in r[\"translation_id\"])}')
print(f'leaf rows (dotted):   {sum(1 for r in rows if \".\" in r[\"translation_id\"])}')
"
```

Expected:
- `rows` matches Task 5 step 6 count
- `columns` is `['id', 'translation_id', 'english', 'kinyarwanda', 'kirundi', 'somali']`
- `first/last id` is `EHEZA-T-0001 .. EHEZA-T-NNNN`
- header + leaf counts add up to total

- [ ] **Step 5: Ask user before committing**

Tell the user:
> "Translate-master pass complete. NNNN rows in `docs/ocl/eheza-concepts-translate.csv`, methodology doc with metadata, README section. Plus the spec and plan from this brainstorming/writing-plans cycle. Five files total ready for one combined commit. May I commit?"

Wait for explicit yes.

- [ ] **Step 6: Single combined commit**

```bash
git add docs/ocl/eheza-concepts-translate.csv \
        docs/ocl/eheza-concepts-translate.md \
        docs/ocl/README.md \
        docs/superpowers/specs/2026-04-19-eheza-concepts-translate-master-design.md \
        docs/superpowers/plans/2026-04-19-eheza-concepts-translate-master.md
git commit -m "$(cat <<'EOF'
Add E-Heza concepts translate master inventory

NNNN rows enumerating user-facing labels from client/src/elm/Translate.elm's
type TranslationId block, after a refined heuristic filter that drops UI
noise (Title/Helper/Page/Button suffixes; Add/Click/Save prefixes;
exact-name UI atoms; union-arg constructors wrapping UI-only union types)
while keeping clinical labels via explicit KEEP exceptions for *Label,
*Question, and *Warning suffixes.

Catalogues derived clinical concepts (Gravida, BMI, ApgarScore, ...) that
the structural master at docs/ocl/eheza-concepts-master.csv can't surface
because they exist only as UI labels, not as Elm record fields or union
constructors.

EHEZA-T-NNNN id space, sequential in source order over emitted rows
(header rows for union-arg constructors + leaf rows for each case branch).
IDs are now immutable; future Translate.elm additions append rows with
the next free id, regardless of source-order position.

Single combined commit covers spec, plan, methodology doc, master CSV,
and a new section in docs/ocl/README.md introducing the labels inventory
alongside the existing structural master. Comparison/decision pass
(use master vs translate vs combine) is deferred to a separate brainstorm
once both files exist.

No edits to existing per-encounter PIH-mapped CSVs, the structural master,
or the released OCL source v1.1-add-patient-registration.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)" && git log -1 --stat
```

Replace `NNNN` in both the message body and the headline with the real row count.

Expected: commit succeeds; `git log -1 --stat` shows 5 files changed with the row counts in the right ballpark (CSV is the bulk).

- [ ] **Step 7: Print final summary**

Tell the user:
> "Translate master pass complete. Commit: \`<sha>\`. NNNN rows in `docs/ocl/eheza-concepts-translate.csv`, methodology doc + README section in place. Branch \`docs/eheza-concepts-master\` is now at N commits ahead of origin. Comparison-vs-structural-master is the next brainstorm when you're ready."
