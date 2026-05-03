# Translate.elm dedup refactor — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Mechanically dedupe 149 locale-equivalent duplicate-english groups in `client/src/elm/Translate.elm` by routing them through `translationSet`. Add 111 new top-level `TranslationId` anchor constructors for groups that need one. Fix ~5–15 source bugs (duplicate case branches with conflicting translations) using internet research as tiebreaker. Document the 126 diverging groups as follow-ups.

**Architecture:** Pure refactor inside one file (`client/src/elm/Translate.elm`). Each duplicate group's 5-line literal `english/kw/kn/so` block becomes a 1-line `translationSet <Anchor>` dispatch. New anchor constructors get added in alphabetical position to the `type TranslationId` declaration AND the main `translationSet` function. Verification per commit: elm-format, elm compile, and a Python script that diffs effective translations before vs after to ensure byte-identical UI behavior.

**Tech Stack:** Elm 0.19.1, `elm-format`, `elm` (compiler), Python 3 for the analysis + cross-check scripts.

**Spec:** `docs/superpowers/specs/2026-04-20-translate-elm-dedup-design.md`

**Conventions enforced throughout:**
- Per global preference: **always ask the user before running `git commit` or `git push`** — every commit step asks first.
- Per CLAUDE.md: doc-only commits get `[ci skip]`; **non-doc commits do NOT get `[ci skip]`** (CI verification is critical for the Elm code changes).
- Per spec §Verification: each non-doc commit must pass elm-format + elm compile + the cross-check translation diff before commit.
- Per spec §Refactor mechanics: anchor naming is PascalCase-of-english; clashes/digit-leading/overlong names get skipped + flagged.
- Per spec §Out of scope: NO changes outside `client/src/elm/Translate.elm` (and the `docs/superpowers/` spec/plan/follow-up doc files).

---

## File Structure

**Modify (committed):**
- `client/src/elm/Translate.elm` — bulk refactor; add ~111 new constructors + case branches; collapse ~370 literal blocks to `translationSet` dispatches; fix ~5–15 source bugs.

**Create (committed):**
- `docs/superpowers/specs/2026-04-20-translate-elm-dedup-design.md` — already written, uncommitted.
- `docs/superpowers/plans/2026-04-20-translate-elm-dedup.md` — this file, uncommitted.
- `docs/superpowers/follow-ups-translate-elm-bugs.md` — list of 126 diverging groups + skipped clash/overlong cases + structural anomalies for future review.

**Working / scratch (NOT committed):**
- `/tmp/translate-dedup-analysis.py` — duplicate-group analysis script.
- `/tmp/translate-dedup-groups.json` — groups data: safe (149) split into needs-anchor (111) / has-anchor (38), plus diverging (126) and source bugs.
- `/tmp/translate-cross-check.py` — verification script comparing effective translations before vs after the refactor.
- `/tmp/translate-pre-snapshot.json` — captured pre-refactor effective translations per top-level constructor.

**Already done before this plan starts:**
- Branch `refactor/translate-elm-dedup` created off `origin/develop` (controller did this during brainstorming).
- Spec at `docs/superpowers/specs/2026-04-20-translate-elm-dedup-design.md` is uncommitted in working tree.

---

## Task 1: Re-run duplicate-group analysis against current `develop`

**Files:**
- Create: `/tmp/translate-dedup-analysis.py` (NOT committed)
- Write: `/tmp/translate-dedup-groups.json` (NOT committed)

The brainstorming explored the duplicate landscape against the docs branch's `Translate.elm`. `develop` may have advanced. Re-run the analysis on the current branch's `Translate.elm` to confirm the counts haven't shifted materially.

- [ ] **Step 1: Save the analysis script verbatim**

Save to `/tmp/translate-dedup-analysis.py`:

```python
#!/usr/bin/env python3
"""Analyze duplicate-english groups in Translate.elm's translationSet function.

Output: JSON with safe groups (locale-equivalent), diverging groups, and source bugs.
"""
import json
import re
import sys
from collections import defaultdict

REPO = "/var/www/html/ihangane"
TRANSLATE_PATH = f"{REPO}/client/src/elm/Translate.elm"


def strip_comments(src):
    src = re.sub(r"\{-[\s\S]*?-\}", "", src)
    src = re.sub(r"--[^\n]*", "", src)
    return src


def find_function_body(src, fn_name):
    """Find the body of `<fn_name> ... =\\n    case ... of\\n` and return (start, end)."""
    m = re.search(rf"^{re.escape(fn_name)}\s+\w+\s*=\s*\n\s*case\s+\w+\s+of\s*\n", src, re.MULTILINE)
    if not m:
        return None
    body_start = m.end()
    end_m = re.search(r"^[a-z]\w*\s*[:=]", src[body_start:], re.MULTILINE)
    body_end = body_start + (end_m.start() if end_m else len(src) - body_start)
    return body_start, body_end


def parse_top_level_constructors(src):
    """Extract list of top-level TranslationId constructor names."""
    m = re.search(r"^type TranslationId\s*\n", src, re.MULTILINE)
    if not m:
        sys.exit("can't find type TranslationId")
    block_start = m.end()
    end_m = re.search(r"^[a-z]\w*\s*[:=]", src[block_start:], re.MULTILINE)
    block_end = block_start + (end_m.start() if end_m else len(src) - block_start)
    block = src[block_start:block_end]
    return set(re.findall(r"^\s*[|=]\s*([A-Z]\w*)", block, re.MULTILINE))


def parse_records_with_constructors(body):
    """For every literal `{ english=..., ... }` in body, find the nearest preceding `<Name> ->`."""
    arrow_pattern = re.compile(r"^( *)([A-Z]\w*)(?:\s+\w+)*\s*->\s*$", re.MULTILINE)
    arrows = [(m.start(), m.group(1), m.group(2)) for m in arrow_pattern.finditer(body)]

    record_pattern = re.compile(
        r"\{\s*english\s*=\s*\"((?:\\.|[^\"\\])*)\""
        r"(?:\s*,\s*kinyarwanda\s*=\s*(Just\s*\"((?:\\.|[^\"\\])*)\"|Nothing))?"
        r"(?:\s*,\s*kirundi\s*=\s*(Just\s*\"((?:\\.|[^\"\\])*)\"|Nothing))?"
        r"(?:\s*,\s*somali\s*=\s*(Just\s*\"((?:\\.|[^\"\\])*)\"|Nothing))?",
    )
    records = []
    for rm in record_pattern.finditer(body):
        pos = rm.start()
        en = rm.group(1)
        rw = rm.group(3) if rm.group(3) is not None else None
        rn = rm.group(5) if rm.group(5) is not None else None
        so = rm.group(7) if rm.group(7) is not None else None
        nearest = None
        for ap, _, an in arrows:
            if ap < pos:
                nearest = an
            else:
                break
        if nearest:
            records.append((nearest, en, rw, rn, so, pos))
    return records


def main():
    src = strip_comments(open(TRANSLATE_PATH).read())
    top_level = parse_top_level_constructors(src)
    fn_range = find_function_body(src, "translationSet")
    if not fn_range:
        sys.exit("can't find translationSet function")
    body = src[fn_range[0]:fn_range[1]]
    records = parse_records_with_constructors(body)
    print(f"Found {len(records)} literal records in translationSet function", file=sys.stderr)
    print(f"Top-level TranslationId constructors: {len(top_level)}", file=sys.stderr)

    # Group by english
    by_en = defaultdict(list)
    for rec in records:
        by_en[rec[1]].append(rec)

    safe_with_anchor = []        # 38-ish: anchor exists at top level
    safe_needs_anchor = []       # 111-ish: needs new anchor
    diverging = []               # 126-ish: locales differ
    source_bugs = []             # constructor appears 2+ times in same group

    for en, recs in by_en.items():
        if len(recs) < 2:
            continue
        # Check locale equivalence
        locale_sets = set((r[2], r[3], r[4]) for r in recs)
        # Detect source bugs: same constructor appears twice
        ctor_counts = defaultdict(int)
        for r in recs:
            ctor_counts[r[0]] += 1
        bug_ctors = [c for c, n in ctor_counts.items() if n >= 2]

        if bug_ctors:
            source_bugs.append({
                "english": en,
                "constructors": [r[0] for r in recs],
                "duplicate_constructors": bug_ctors,
                "locale_sets": [list(s) for s in locale_sets],
                "records": [{"ctor": r[0], "rw": r[2], "rn": r[3], "so": r[4]} for r in recs],
            })
            continue  # source bugs are tracked separately

        if len(locale_sets) == 1:
            # Safe — find anchor candidate (PascalCase of english)
            anchor_name = pascalize(en)
            if anchor_name is None:
                # Skip-and-flag (digit-leading, overlong, empty)
                diverging.append({"english": en, "constructors": [r[0] for r in recs],
                                  "skip_reason": "anchor naming infeasible"})
                continue
            existing_anchor = next((r[0] for r in recs if r[0] == anchor_name and r[0] in top_level), None)
            if existing_anchor:
                safe_with_anchor.append({
                    "english": en, "anchor": existing_anchor,
                    "constructors": [r[0] for r in recs],
                    "rw": recs[0][2], "rn": recs[0][3], "so": recs[0][4],
                })
            elif anchor_name in top_level:
                # Anchor name clashes with existing constructor that has DIFFERENT translations
                diverging.append({"english": en, "constructors": [r[0] for r in recs],
                                  "skip_reason": f"clash with existing top-level {anchor_name}"})
            else:
                safe_needs_anchor.append({
                    "english": en, "anchor": anchor_name,
                    "constructors": [r[0] for r in recs],
                    "rw": recs[0][2], "rn": recs[0][3], "so": recs[0][4],
                })
        else:
            diverging.append({
                "english": en, "constructors": [r[0] for r in recs],
                "locale_sets": [list(s) for s in locale_sets],
                "records": [{"ctor": r[0], "rw": r[2], "rn": r[3], "so": r[4]} for r in recs],
            })

    out = {
        "safe_with_anchor": safe_with_anchor,
        "safe_needs_anchor": safe_needs_anchor,
        "diverging": diverging,
        "source_bugs": source_bugs,
        "summary": {
            "safe_with_anchor": len(safe_with_anchor),
            "safe_needs_anchor": len(safe_needs_anchor),
            "diverging": len(diverging),
            "source_bugs": len(source_bugs),
            "top_level_constructors": len(top_level),
            "total_literal_records": len(records),
        },
    }
    json.dump(out, sys.stdout, indent=2)
    print(file=sys.stderr)
    print("SUMMARY:", json.dumps(out["summary"], indent=2), file=sys.stderr)


def pascalize(en):
    """PascalCase of english label. Return None if infeasible (digit-leading, empty, too long, has bad chars)."""
    if not en or en.strip() == "":
        return None
    # Strip parens content's interior chars but keep digits ("(250mg)" → "250mg")
    cleaned = re.sub(r"[^\w\s]", "", en)
    parts = cleaned.split()
    if not parts:
        return None
    name = "".join(p[0].upper() + p[1:] for p in parts)
    if not name or name[0].isdigit():
        return None
    if len(name) > 50:
        return None
    return name


if __name__ == "__main__":
    main()
```

- [ ] **Step 2: Verify the script parses**

```bash
python3 -m py_compile /tmp/translate-dedup-analysis.py && echo "ok"
```

Expected: `ok`. Any SyntaxError → typo, fix.

- [ ] **Step 3: Run the analysis**

```bash
cd /var/www/html/ihangane && python3 /tmp/translate-dedup-analysis.py > /tmp/translate-dedup-groups.json 2> /tmp/translate-dedup-summary.txt
echo "exit=$?"
cat /tmp/translate-dedup-summary.txt
```

Expected:
- `exit=0`
- Summary in stderr lists counts. Brainstorming exploration counts (149 safe / 111 needs-anchor / 38 has-anchor / 126 diverging / a few source bugs) — current `develop` numbers should be in the same ballpark. If wildly different (>20% delta on safe-group count), STOP and report to controller — develop has shifted significantly and the design assumptions may need revisiting.

- [ ] **Step 4: Quick sanity-check the JSON**

```bash
python3 -c "
import json
d = json.load(open('/tmp/translate-dedup-groups.json'))
for k, v in d['summary'].items():
    print(f'  {k}: {v}')
print()
print('Sample safe-with-anchor (3):')
for g in d['safe_with_anchor'][:3]:
    print(f\"  english='{g['english']}' anchor={g['anchor']} ctors={g['constructors']}\")
print()
print('Sample safe-needs-anchor (3):')
for g in d['safe_needs_anchor'][:3]:
    print(f\"  english='{g['english']}' new_anchor={g['anchor']} ctors={g['constructors']}\")
print()
print('Source bugs:')
for g in d['source_bugs']:
    print(f\"  english='{g['english']}' duplicate_ctors={g['duplicate_constructors']}\")
"
```

Expected: counts in line with brainstorming exploration; samples look like the kinds of groups expected.

NO COMMIT (scratch files only).

---

## Task 2: Capture pre-refactor translations baseline

**Files:**
- Create: `/tmp/translate-cross-check.py` (NOT committed)
- Write: `/tmp/translate-pre-snapshot.json` (NOT committed)

The cross-check verification needs a baseline: for every TranslationId, what does it currently render? Capture that BEFORE any edits so we can compare AFTER.

- [ ] **Step 1: Save the cross-check script**

Save to `/tmp/translate-cross-check.py`:

```python
#!/usr/bin/env python3
"""Capture or verify the effective translations of all top-level TranslationIds.

Mode 'snapshot': writes JSON to stdout — for each top-level ctor, its (en, rw, rn, so).
Mode 'compare':  reads two snapshots and reports any differences.

Usage:
    python3 /tmp/translate-cross-check.py snapshot > /tmp/translate-pre-snapshot.json
    python3 /tmp/translate-cross-check.py snapshot > /tmp/translate-post-snapshot.json
    python3 /tmp/translate-cross-check.py compare /tmp/translate-pre-snapshot.json /tmp/translate-post-snapshot.json
"""
import json
import re
import sys

REPO = "/var/www/html/ihangane"
TRANSLATE_PATH = f"{REPO}/client/src/elm/Translate.elm"


def strip_comments(src):
    src = re.sub(r"\{-[\s\S]*?-\}", "", src)
    src = re.sub(r"--[^\n]*", "", src)
    return src


def find_translationset_body(src):
    m = re.search(r"^translationSet\s+\w+\s*=\s*\n\s*case\s+\w+\s+of\s*\n", src, re.MULTILINE)
    if not m:
        sys.exit("translationSet not found")
    body_start = m.end()
    end_m = re.search(r"^[a-z]\w*\s*[:=]", src[body_start:], re.MULTILINE)
    return src[body_start:body_start + (end_m.start() if end_m else len(src) - body_start)]


def parse_branches(body):
    """Walk top-level case branches in the translationSet body. Returns dict ctor -> kind+payload."""
    arrow_pattern = re.compile(r"^( {8})([A-Z]\w*)(?:\s+\w+)?\s*->\s*\n", re.MULTILINE)
    arrows = list(arrow_pattern.finditer(body))
    branches = {}
    for i, m in enumerate(arrows):
        ctor = m.group(2)
        end = arrows[i + 1].start() if i + 1 < len(arrows) else len(body)
        branch_body = body[m.end():end].rstrip()
        # Dispatch?
        d = re.match(r"^\s+translationSet\s+(\w+)\s*$", branch_body)
        if d:
            branches[ctor] = {"kind": "dispatch", "to": d.group(1)}
            continue
        # Literal record?
        rm = re.search(
            r"\{\s*english\s*=\s*\"((?:\\.|[^\"\\])*)\""
            r"(?:\s*,\s*kinyarwanda\s*=\s*(Just\s*\"((?:\\.|[^\"\\])*)\"|Nothing))?"
            r"(?:\s*,\s*kirundi\s*=\s*(Just\s*\"((?:\\.|[^\"\\])*)\"|Nothing))?"
            r"(?:\s*,\s*somali\s*=\s*(Just\s*\"((?:\\.|[^\"\\])*)\"|Nothing))?",
            branch_body,
        )
        if rm:
            branches[ctor] = {
                "kind": "literal",
                "en": rm.group(1),
                "rw": rm.group(3),
                "rn": rm.group(5),
                "so": rm.group(7),
            }
            continue
        # Other (case-of inside)
        branches[ctor] = {"kind": "other", "preview": branch_body[:80].replace("\n", " ")}
    return branches


def resolve(branches, ctor, depth=0):
    """Follow dispatches to get the effective (en, rw, rn, so) tuple."""
    if depth > 10:
        return ("__CYCLE__", None, None, None)
    if ctor not in branches:
        return ("__MISSING__", None, None, None)
    b = branches[ctor]
    if b["kind"] == "literal":
        return (b["en"], b["rw"], b["rn"], b["so"])
    if b["kind"] == "dispatch":
        return resolve(branches, b["to"], depth + 1)
    return ("__OTHER__", None, None, None)


def snapshot():
    src = strip_comments(open(TRANSLATE_PATH).read())
    body = find_translationset_body(src)
    branches = parse_branches(body)
    out = {}
    for ctor in branches:
        out[ctor] = resolve(branches, ctor)
    json.dump(out, sys.stdout, indent=2, sort_keys=True)


def compare(pre_path, post_path):
    pre = json.load(open(pre_path))
    post = json.load(open(post_path))
    only_pre = set(pre) - set(post)
    only_post = set(post) - set(pre)
    diffs = []
    for k in sorted(set(pre) & set(post)):
        if pre[k] != post[k]:
            diffs.append((k, pre[k], post[k]))
    print(f"Pre-only constructors:   {len(only_pre)}")
    print(f"Post-only constructors:  {len(only_post)}  (expected: ~111 new anchors)")
    print(f"Translation diffs:       {len(diffs)}  (expected: 0)")
    if only_pre:
        print(f"  Sample pre-only:  {sorted(only_pre)[:5]}")
    if only_post:
        print(f"  Sample post-only: {sorted(only_post)[:5]}")
    if diffs:
        print("CRITICAL: translations changed — should be 0 differences:")
        for k, a, b in diffs[:10]:
            print(f"  {k}: {a!r} -> {b!r}")
        sys.exit(1)
    print("PASS: zero translation drift.")


if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit("usage: snapshot | compare <pre> <post>")
    if sys.argv[1] == "snapshot":
        snapshot()
    elif sys.argv[1] == "compare":
        compare(sys.argv[2], sys.argv[3])
    else:
        sys.exit("unknown mode")
```

- [ ] **Step 2: Verify the script parses**

```bash
python3 -m py_compile /tmp/translate-cross-check.py && echo "ok"
```

Expected: `ok`.

- [ ] **Step 3: Capture the pre-refactor snapshot**

```bash
cd /var/www/html/ihangane && python3 /tmp/translate-cross-check.py snapshot > /tmp/translate-pre-snapshot.json
echo "captured $(python3 -c "import json; print(len(json.load(open('/tmp/translate-pre-snapshot.json'))))") effective translations"
```

Expected: count is ~1860 (matching the top-level TranslationId constructor count from brainstorming).

NO COMMIT.

---

## Task 3: Commit spec + plan

**Files:**
- Stage: `docs/superpowers/specs/2026-04-20-translate-elm-dedup-design.md`
- Stage: `docs/superpowers/plans/2026-04-20-translate-elm-dedup.md`

- [ ] **Step 1: Verify both files are uncommitted**

```bash
git status -- docs/superpowers/
```

Expected: both files listed as untracked.

- [ ] **Step 2: Ask user before committing**

> "Spec and plan are uncommitted. May I commit them as one combined commit (with `[ci skip]` since docs-only)?"

Wait for explicit yes.

- [ ] **Step 3: Commit**

```bash
git add docs/superpowers/specs/2026-04-20-translate-elm-dedup-design.md \
        docs/superpowers/plans/2026-04-20-translate-elm-dedup.md
git commit -m "$(cat <<'EOF'
Add Translate.elm dedup refactor spec and implementation plan

Spec covers mechanically deduping 149 locale-equivalent duplicate-english
groups in Translate.elm by routing them through translationSet. Plan
decomposes into 7 tasks: re-analyze duplicate groups -> capture
pre-refactor snapshot -> commit spec/plan -> add 111 new top-level
anchor TranslationIds -> refactor 149 groups to translationSet -> fix
~5-15 source bugs with internet-research tiebreakers -> document 126
diverging groups + skipped cases for follow-up native-speaker review.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)" && git log -1 --oneline
```

Expected: commit succeeds.

---

## Task 4: Add 111 new top-level `TranslationId` anchors

**Files:**
- Modify: `client/src/elm/Translate.elm` (additions only — no deletions, no replacements)

- [ ] **Step 1: Build the list of new anchors to add**

```bash
python3 -c "
import json
d = json.load(open('/tmp/translate-dedup-groups.json'))
needs_anchor = d['safe_needs_anchor']
print(f'{len(needs_anchor)} new anchors to add')
print()
print('Sample:')
for g in needs_anchor[:10]:
    print(f\"  {g['anchor']}: english='{g['english']}' rw={g['rw']!r} rn={g['rn']!r} so={g['so']!r}\")
"
```

Expected: ~111 anchors listed; sample shows the (anchor_name, english, rw, rn, so) tuples that will be added.

- [ ] **Step 2: Find insertion points + add the new constructors to `type TranslationId`**

The `type TranslationId` declaration spans lines 329–2192 of source (per spec). For each new anchor, find the alphabetical insertion point in the type declaration body. Insert the constructor name in PascalCase order. Use Edit tool with concrete `old_string` / `new_string` per insertion (one Edit call per anchor — this is mechanical and many calls but fine).

For example, if inserting `NoneOfTheAbove` between `NoneOfTheAboveItem` (line N) and `NormalAbdomen` (line N+1):

```
old_string: '    | NoneOfTheAboveItem\n    | NormalAbdomen'
new_string: '    | NoneOfTheAboveItem\n    | NoneOfTheAbove\n    | NormalAbdomen'
```

(If the existing constructor list isn't strictly alphabetical at the insertion point, insert the new constructor in the closest plausible alphabetical spot — minor non-strictness is OK; the goal is "roughly alphabetical, not adjacent to a totally unrelated section".)

- [ ] **Step 3: Add the 111 new case branches in `translationSet`**

For each new anchor, add a literal case branch in the main `translationSet` function (which starts at line 2192 in current source). Same alphabetical insertion principle. Each case branch is a 5-line literal:

```elm
        NoneOfTheAbove ->
            { english = "None of the Above"
            , kinyarwanda = Just "..."
            , kirundi = Just "..."
            , somali = Just "..."
            }
```

Use the rw/rn/so values from the analysis JSON's `safe_needs_anchor` entries (these are the agreed-upon translations that all duplicates already share — adding the anchor with these values doesn't change UI behavior for any existing call site).

For locales that are `null` in the JSON (no Just value in the originals), use `Nothing`:

```elm
            , kinyarwanda = Nothing
```

- [ ] **Step 4: Run elm-format**

```bash
cd /var/www/html/ihangane/client && ./node_modules/.bin/elm-format --validate src/elm/Translate.elm
echo "exit=$?"
```

Expected: exit=0. Any non-zero = formatting issue. Re-run with `--yes` to auto-format if needed:
```bash
./node_modules/.bin/elm-format src/elm/Translate.elm --yes
```

- [ ] **Step 5: Run elm compile**

```bash
cd /var/www/html/ihangane/client && ./node_modules/.bin/elm make src/elm/Main.elm --output=/dev/null
echo "exit=$?"
```

Expected: exit=0. Compile errors = the new constructors broke the type definition somehow; debug.

- [ ] **Step 6: Cross-check translations**

Capture post-add snapshot and compare:

```bash
cd /var/www/html/ihangane && python3 /tmp/translate-cross-check.py snapshot > /tmp/translate-post-add-snapshot.json
python3 /tmp/translate-cross-check.py compare /tmp/translate-pre-snapshot.json /tmp/translate-post-add-snapshot.json
```

Expected:
- Pre-only: 0
- Post-only: ~111 (the new anchors, exactly the count of `safe_needs_anchor`)
- Translation diffs: **0**
- Final line: `PASS: zero translation drift.`

If any non-zero diff appears, the additions changed an existing constructor's resolution somehow — debug, fix, re-run.

- [ ] **Step 7: Ask user before committing**

> "Added 111 new top-level TranslationId anchors to Translate.elm. elm-format clean, elm compile clean, cross-check shows 0 translation drift. May I commit?"

Wait for explicit yes.

- [ ] **Step 8: Commit (no `[ci skip]`)**

```bash
git add client/src/elm/Translate.elm
git commit -m "$(cat <<'EOF'
Add 111 new top-level TranslationId anchors for dedup refactor

These anchors have the canonical translations (en/rw/rn/so) shared by
their soon-to-be-deduplicated call sites. No existing TranslationId is
modified; no UI behavior changes from this commit alone (each new
anchor is a fresh constructor with no callers yet).

Cross-check confirmed 0 translation drift across all 1860 pre-existing
top-level TranslationIds.

The actual translationSet dispatch refactor follows in the next commit.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)" && git log -1 --stat
```

Expected: commit succeeds.

---

## Task 5: Refactor 149 groups → `translationSet <Anchor>` calls

**Files:**
- Modify: `client/src/elm/Translate.elm` (replacements — collapsing 5-line literals to 1-line dispatches)

- [ ] **Step 1: Build the list of replacements**

```bash
python3 -c "
import json
d = json.load(open('/tmp/translate-dedup-groups.json'))
all_safe = d['safe_with_anchor'] + d['safe_needs_anchor']
total_replacements = sum(len(g['constructors']) - (1 if g in d['safe_with_anchor'] else 0) for g in all_safe)
# Each safe_with_anchor: replace all N constructors INCLUDING the anchor (no, the anchor stays as literal)
#   Actually: the existing anchor stays literal; only the OTHER N-1 constructors become dispatches
# Each safe_needs_anchor: anchor was just added in Task 4 (literal); the original N constructors all become dispatches
# Build the work list
to_replace = []
for g in d['safe_with_anchor']:
    for ctor in g['constructors']:
        if ctor != g['anchor']:
            to_replace.append((ctor, g['anchor'], g['english']))
for g in d['safe_needs_anchor']:
    for ctor in g['constructors']:
        to_replace.append((ctor, g['anchor'], g['english']))
print(f'Total constructors to refactor (collapse to translationSet): {len(to_replace)}')
print()
print('Sample:')
for c, a, e in to_replace[:10]:
    print(f'  {c} -> translationSet {a}   (english={e!r})')
"
```

Expected: ~370 replacements listed (the 686 minus the 38 anchors that were already top-level + the 111 new anchors that we just added as literals).

- [ ] **Step 2: For each constructor, replace its 5-line literal block with `translationSet <Anchor>`**

For each (ctor, anchor) pair from the work list, locate its case branch in the `translationSet` function. The branch looks like:

```elm
        OutcomeOther ->
            { english = "Other"
            , kinyarwanda = Just "Ibindi"
            , kirundi = Just "Ibindi"
            , somali = Just "Kale"
            }
```

Replace with:

```elm
        OutcomeOther ->
            translationSet Other
```

Use the Edit tool — exact string match on the full 6-line block (including the `Ctor ->` line and the closing brace), replaced with the 2-line dispatch.

This is many edits (~370). Batch them but verify each lands correctly. If a constructor's branch doesn't match the expected literal pattern (e.g., it has a comment inside, or unusual whitespace), skip it for this commit and document the skip in the commit message.

- [ ] **Step 3: Run elm-format**

```bash
cd /var/www/html/ihangane/client && ./node_modules/.bin/elm-format --validate src/elm/Translate.elm
```

Expected: exit=0. Auto-fix if needed: `./node_modules/.bin/elm-format src/elm/Translate.elm --yes`.

- [ ] **Step 4: Run elm compile**

```bash
cd /var/www/html/ihangane/client && ./node_modules/.bin/elm make src/elm/Main.elm --output=/dev/null
```

Expected: exit=0.

- [ ] **Step 5: Cross-check translations**

```bash
cd /var/www/html/ihangane && python3 /tmp/translate-cross-check.py snapshot > /tmp/translate-post-refactor-snapshot.json
python3 /tmp/translate-cross-check.py compare /tmp/translate-pre-snapshot.json /tmp/translate-post-refactor-snapshot.json
```

Expected:
- Pre-only: 0
- Post-only: ~111 (the new anchors added in Task 4)
- Translation diffs: **0** — this is the critical check; the refactor must NOT have changed any existing constructor's effective translation
- Final line: `PASS: zero translation drift.`

If any diff > 0:
- Look at the diff details to find which constructor's translation changed
- Most likely cause: the refactor pointed a constructor at the wrong anchor (typo) OR the anchor's literal differs from what the original literal had
- Fix the offending dispatch, re-run validation

- [ ] **Step 6: Ask user before committing**

> "Refactored ~370 duplicate literals to translationSet dispatches. elm-format clean, elm compile clean, cross-check confirms 0 translation drift. May I commit?"

Wait for explicit yes.

- [ ] **Step 7: Commit (no `[ci skip]`)**

```bash
git add client/src/elm/Translate.elm
git commit -m "$(cat <<'EOF'
Refactor 149 duplicate-english groups to use translationSet

Collapses ~370 duplicate 5-line literal { english=..., kw=..., kn=...,
so=... } blocks down to 1-line `translationSet <Anchor>` dispatches.
The anchors are either pre-existing top-level TranslationIds (38
groups) or the new anchors added in the previous commit (111 groups).

Cross-check verified zero translation drift — every constructor's
effective (en, kw, kn, so) tuple is byte-identical pre-vs-post refactor.

Source bugs (constructors appearing 2+ times in the same case-of with
divergent translations) are intentionally NOT touched in this commit;
they get fixed individually in the next commit(s) with internet-research
tiebreakers.

Diverging groups (where the same english label has different rw/rn/so
across duplicates — refactoring would change UI behavior for some
users) are listed in docs/superpowers/follow-ups-translate-elm-bugs.md
for native-speaker review; not refactored in this PR.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)" && git log -1 --stat
```

Expected: commit succeeds; large net-negative line delta (each 6-line block → 2-line dispatch).

---

## Task 6: Fix source bugs (one commit per logical bug)

**Files:**
- Modify: `client/src/elm/Translate.elm` (delete duplicate case branches; canonicalize translations)

For each `source_bug` entry in `/tmp/translate-dedup-groups.json`, do the following:

- [ ] **Step 1: List the source bugs**

```bash
python3 -c "
import json
d = json.load(open('/tmp/translate-dedup-groups.json'))
print(f'Source bugs: {len(d[\"source_bugs\"])}')
for i, b in enumerate(d['source_bugs'], 1):
    print(f'{i}. english={b[\"english\"]!r}')
    print(f'   duplicate constructors: {b[\"duplicate_constructors\"]}')
    print(f'   distinct locale tuples: {len(b[\"locale_sets\"])}')
    for r in b['records']:
        print(f'     {r[\"ctor\"]}  rw={r[\"rw\"]!r} rn={r[\"rn\"]!r} so={r[\"so\"]!r}')
    print()
"
```

Expected: ~5–15 bugs listed. For each: the english label, which constructors are duplicated within the same case-of, and the conflicting translations.

For each bug (Steps 2–9 below repeat per bug):

- [ ] **Step 2: Web-search the english label translation**

Use WebSearch tool to look up the translation in each affected locale. Example queries:
- `"<english label>" Kinyarwanda translation medical`
- `"<english label>" Kirundi translation`
- `"<english label>" Somali translation medical`

Capture the canonical translation per locale based on the search results. Reputable sources: Glosbe, Indifferent Languages, medical-specific dictionaries.

- [ ] **Step 3: Pick the canonical translation per locale**

Given the conflicting in-source translations + the web research:
- If web research clearly favors one of the existing translations: use that.
- If neither in-source matches well: prefer the more clinically-precise of the two existing translations.
- Document the reasoning in the upcoming commit message.

- [ ] **Step 4: Edit `Translate.elm` to delete the duplicate case branch + canonicalize the first**

Find the duplicate case branch (the second occurrence of `<Ctor> ->`) in the case-of block and **delete it entirely** (the `Ctor ->` line + the literal record block).

Then update the FIRST occurrence's translations to use the canonical values picked in Step 3.

- [ ] **Step 5: Run elm-format + elm compile**

```bash
cd /var/www/html/ihangane/client && ./node_modules/.bin/elm-format --validate src/elm/Translate.elm && ./node_modules/.bin/elm make src/elm/Main.elm --output=/dev/null
echo "exit=$?"
```

Expected: exit=0.

- [ ] **Step 6: Cross-check (this WILL show 1 diff for the bug being fixed)**

```bash
cd /var/www/html/ihangane && python3 /tmp/translate-cross-check.py snapshot > /tmp/translate-post-bugfix-snapshot.json
python3 /tmp/translate-cross-check.py compare /tmp/translate-pre-snapshot.json /tmp/translate-post-bugfix-snapshot.json
```

Expected:
- Translation diffs: **N** where N is the number of bug fixes applied so far (the bug-fix DOES intentionally change translations to the canonical values)
- The diffs listed should all be from constructors involved in source bugs we've fixed
- No unrelated constructors should appear in the diff

If unrelated constructors appear in the diff, the bug-fix accidentally affected something else; investigate.

- [ ] **Step 7: Ask user before committing**

> "Fixed source bug for english=<label>. Removed duplicate case branch, canonicalized translations to: rw=<X>, rn=<Y>, so=<Z>. Web research notes: <summary>. May I commit?"

Wait for explicit yes.

- [ ] **Step 8: Commit (no `[ci skip]`)**

```bash
git add client/src/elm/Translate.elm
git commit -m "$(cat <<'EOF'
Fix duplicate case branch for <Constructor> with canonical translations

The case branch `<Ctor> ->` appeared twice in the same case-of block
in Translate.elm with conflicting <locale> translations. The Elm
compiler tolerated this (the second branch was unreachable), but the
fact that two different translations existed for the same constructor
indicated a real bug.

Canonical translations picked via web research:
  english:    "<en>"
  kinyarwanda: "<rw>"  (was "<rw_a>" / "<rw_b>")
  kirundi:    "<rn>"  (was "<rn_a>" / "<rn_b>")
  somali:     "<so>"

Sources consulted:
  - <URL 1>
  - <URL 2>

The duplicate branch is deleted; the first occurrence is updated to
the canonical translations.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)" && git log -1 --stat
```

(Substitute real `<placeholder>` values from the bug being fixed.)

Expected: commit succeeds.

- [ ] **Step 9: Loop — repeat Steps 2–8 for each remaining bug**

After processing all bugs, the cross-check final diff count should equal exactly the number of bug fixes committed.

---

## Task 7: Write the follow-up doc

**Files:**
- Create: `docs/superpowers/follow-ups-translate-elm-bugs.md`

- [ ] **Step 1: Write the follow-up doc**

Create `docs/superpowers/follow-ups-translate-elm-bugs.md` with these sections:

```markdown
# Translate.elm — follow-up items

Items surfaced during the `refactor/translate-elm-dedup` work that need
review beyond the scope of that PR. None block normal use of E-Heza;
all are quality / cleanliness opportunities.

## Diverging duplicate-english groups (need native-speaker review)

These N groups have the same English label but different translations
in at least one of rw/rn/so across their duplicate constructors. They
were NOT refactored to `translationSet` because doing so would change
the UI translation for some users. A native-speaker reviewer should
pick the canonical per-locale translation for each group, then a
follow-up PR can dedup them the same way as the safe groups.

| english | constructors | divergence (sample) |
|---|---|---|
| ... one row per diverging group, populated from /tmp/translate-dedup-groups.json's `diverging` list ... |

## Skipped during dedup refactor

Cases where the dedup refactor would have triggered but was skipped:

### Anchor-naming infeasible

Cases where the proposed PascalCase anchor name would be:
- Empty (english is `""`)
- Digit-leading (would be invalid Elm constructor)
- > 50 characters (e.g., "1 tablet by mouth twice a day" → too verbose)

| english | constructors | reason skipped |
|---|---|---|
| ... populated from the `diverging` list entries with `skip_reason` |

### Anchor name clash with existing constructor

Cases where the proposed anchor name (PascalCase of english) would
clash with an existing top-level TranslationId that has DIFFERENT
translations.

| english | proposed anchor (clashes) | constructors | existing-anchor translations differ how |
|---|---|---|---|

## Constructor-rename candidates (typos)

Bugs revealed during exploration that involve renaming Elm constructors
(out of scope because renames change call sites across the codebase).

- `NitritionSigns` → `NutritionSigns` (typo). Currently both forms exist
  with different ids; the typo'd form has english="Nutrition Signs".

(Add others discovered during exploration.)

## Empty-string english groups

Groups where english is `""`. Likely intentional in some contexts (e.g.,
`EmptyString`, `DeviceNotAuthorized`'s placeholder). Documented for
completeness; safest to leave alone.
```

Populate the tables from `/tmp/translate-dedup-groups.json`'s `diverging` list using this helper that emits the exact markdown rows to copy in:

```bash
python3 <<'EOF'
import json
d = json.load(open('/tmp/translate-dedup-groups.json'))

print("=== Diverging groups (paste under the Diverging section table) ===")
diverging_real = [g for g in d['diverging'] if 'records' in g]
print(f"Total: {len(diverging_real)}")
for g in diverging_real:
    en = g['english'].replace('|', '\\|')
    ctors = ", ".join(f"`{c}`" for c in g['constructors'])
    # Sample one divergence: show rw values across constructors if they differ
    rws = list(set(r['rw'] for r in g['records']))
    rns = list(set(r['rn'] for r in g['records']))
    sos = list(set(r['so'] for r in g['records']))
    diff_summary_parts = []
    if len(rws) > 1: diff_summary_parts.append(f"rw: {' / '.join(repr(x) for x in rws)}")
    if len(rns) > 1: diff_summary_parts.append(f"rn: {' / '.join(repr(x) for x in rns)}")
    if len(sos) > 1: diff_summary_parts.append(f"so: {' / '.join(repr(x) for x in sos)}")
    diff_summary = "<br>".join(diff_summary_parts).replace('|', '\\|')
    print(f"| {en} | {ctors} | {diff_summary} |")
print()
print("=== Skipped: anchor naming infeasible (paste under that subsection) ===")
infeasible = [g for g in d['diverging'] if 'skip_reason' in g and 'clash' not in g.get('skip_reason', '')]
for g in infeasible:
    en = g['english'].replace('|', '\\|')
    ctors = ", ".join(f"`{c}`" for c in g['constructors'])
    print(f"| {en} | {ctors} | {g['skip_reason']} |")
print()
print("=== Skipped: anchor name clash (paste under that subsection) ===")
clashes = [g for g in d['diverging'] if 'skip_reason' in g and 'clash' in g['skip_reason']]
for g in clashes:
    en = g['english'].replace('|', '\\|')
    ctors = ", ".join(f"`{c}`" for c in g['constructors'])
    print(f"| {en} | {g['skip_reason']} | {ctors} | (review existing anchor's translations) |")
EOF
```

Copy the printed markdown rows into the corresponding tables in the doc. The first table needs the column headers from the template above; the rows from the helper output go below those headers.

- [ ] **Step 2: Render-check the file**

```bash
python3 -c "
import re, pathlib
text = pathlib.Path('docs/superpowers/follow-ups-translate-elm-bugs.md').read_text()
print('headings:', re.findall(r'^##+ .*', text, re.M))
print('total lines:', len(text.splitlines()))
"
```

Expected: 4 `##` sections (Diverging / Skipped / Rename candidates / Empty-string), with `###` sub-headings under Skipped.

- [ ] **Step 3: Ask user before committing**

> "Follow-up doc written listing diverging groups + skipped cases + rename candidates. May I commit (with `[ci skip]` since docs-only)?"

Wait for yes.

- [ ] **Step 4: Commit (with `[ci skip]`)**

```bash
git add docs/superpowers/follow-ups-translate-elm-bugs.md
git commit -m "$(cat <<'EOF'
Add follow-up doc for Translate.elm dedup refactor

Lists items surfaced during the dedup refactor that need review beyond
the scope of that PR:
- 126 diverging duplicate-english groups (need native-speaker review)
- Anchor-naming-infeasible cases (digit-leading, overlong, empty)
- Anchor-name clash cases (collision with existing TranslationId)
- Constructor-rename candidates (typos like NitritionSigns)
- Empty-string english groups (likely intentional)

None block normal use; all are cleanliness / quality opportunities for
follow-up PRs.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)" && git log -1 --stat
```

Expected: commit succeeds.

---

## Task 8: Final verification + push

**Files:** none modified; verification only.

- [ ] **Step 1: Confirm git state**

```bash
git log --oneline refactor/translate-elm-dedup ^origin/develop
```

Expected: 5 commits in order (newest first):
```
... follow-up doc
... source bug fix(es) — one or more
... refactor 149 groups
... add 111 new anchors
... spec + plan
```

- [ ] **Step 2: Working tree clean (apart from pre-existing local mods)**

```bash
git status -- client/ docs/
```

Expected: nothing to commit for `client/` or `docs/`. Pre-existing `.ddev/` / `.gitignore` / `server/RoboFile.php` modifications can stay (they predate this work).

- [ ] **Step 3: Final cross-check (cumulative — all changes since pre-snapshot)**

```bash
cd /var/www/html/ihangane && python3 /tmp/translate-cross-check.py snapshot > /tmp/translate-final-snapshot.json
python3 /tmp/translate-cross-check.py compare /tmp/translate-pre-snapshot.json /tmp/translate-final-snapshot.json
```

Expected:
- Pre-only: 0
- Post-only: ~111 (the new anchors)
- Translation diffs: equal to the number of source-bug fixes committed (each intentionally changed translations to canonical values)
- Diff list contains ONLY the constructors involved in source bug fixes — no others

- [ ] **Step 4: Final elm-format + compile sanity**

```bash
cd /var/www/html/ihangane/client && ./node_modules/.bin/elm-format --validate src/elm/Translate.elm && ./node_modules/.bin/elm make src/elm/Main.elm --output=/dev/null && echo "all green"
```

Expected: `all green`.

- [ ] **Step 5: Net line delta sanity**

```bash
git diff --shortstat origin/develop -- client/src/elm/Translate.elm
```

Expected: net negative; ballpark −1500 to −2000 lines.

- [ ] **Step 6: Ask user about pushing**

> "All 5 commits clean, all verification green. May I push the branch to origin?"

Wait for explicit yes. (Per global preference: always ask before pushing.)

- [ ] **Step 7: Push**

```bash
git push -u origin refactor/translate-elm-dedup
```

Expected: push succeeds; GitHub provides the PR-creation URL in the output.

- [ ] **Step 8: Print final summary to user**

> "Translate.elm dedup refactor complete. Branch `refactor/translate-elm-dedup` pushed with 5 commits. Net delta: ~−<X> lines in Translate.elm. <N> source bugs fixed. Diverging groups + skipped cases documented in `docs/superpowers/follow-ups-translate-elm-bugs.md`. PR creation URL: <url>. CI will run the full Elm + SimpleTest suite on the PR."

NO COMMIT (verification + push only).
