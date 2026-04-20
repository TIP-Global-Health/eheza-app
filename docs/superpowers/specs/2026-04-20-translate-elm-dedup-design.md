# Translate.elm dedup refactor — design

Mechanically dedupes 149 locale-equivalent duplicate-english groups in
`client/src/elm/Translate.elm` by routing them through `translationSet`.
Each duplicate group's literal `english/kw/kn/so` block becomes a single
`translationSet <Anchor>` dispatch line; if no top-level anchor exists,
a new top-level `TranslationId` constructor is added to serve as the
target. Diverging groups (126, where at least one locale string differs
across duplicates — refactoring would change UI behavior) are listed in
a follow-up doc and NOT touched in this pass. Source bugs surfaced
during exploration (~5–15 cases of duplicate case branches with
conflicting translations) are fixed in this PR using internet research
to pick the canonical translation per locale.

## Scope

In: 149 duplicate-english groups in `client/src/elm/Translate.elm`'s
main `translationSet` function where all 4 locales (en/rw/rn/so) agree
across duplicates. Of these:

- 38 groups have an existing top-level `TranslationId` anchor whose
  literal translations match the group → refactor in place.
- 111 groups have no anchor → mint a new top-level `TranslationId`
  constructor with the canonical literal translations, then refactor
  the duplicates.

Plus ~5–15 source bugs (duplicate case branches with conflicting
translations, e.g., `DiagnosisSimpleColdAndCough` appearing twice with
different Kinyarwanda values) — fixed in this PR with internet-research
tiebreakers documented per commit.

Out of scope:

- 126 diverging groups (where ≥ 1 locale string differs across
  duplicates). Refactoring these would change UI behavior; needs
  native-speaker linguistic review per group. Listed in a follow-up
  doc.
- Constructor renames (e.g., the `NitritionSigns` typo) — renaming a
  constructor changes call sites across the codebase, out of scope.
- Empty-string english (`""`) groups — likely intentional placeholders
  in some contexts; skip and document.
- Updates to the labels-master CSV
  (`docs/ocl/eheza-concepts-translate.csv` on the
  `docs/eheza-concepts-master` branch). The labels master walked the
  pre-refactor `Translate.elm`; once this refactor lands in develop
  and merges into the docs branch, the labels master will need a
  re-walk against the new structure. That re-walk is its own concern,
  separate from this PR.
- Any non-Translate.elm changes (no view/model/Update edits).

## Branch and workflow

**Branch:** `refactor/translate-elm-dedup`, branched off `origin/develop`.

**Workspace:** same checkout — switch branches via `git checkout`. The
`docs/eheza-concepts-master` branch (which carries this brainstorming's
ancestor work + the labels-master CSV) is preserved on the local clone
and remote; it does not need to be touched during this refactor.

**Cross-branch interactions:**

- The labels-master CSV will be stale relative to the post-refactor
  `Translate.elm` once this work lands. A re-walk is a separate
  follow-up, not part of this PR.
- `develop` may have advanced since the docs branches were cut. The
  refactor sees whatever's currently in `origin/develop`'s
  `Translate.elm`. If the duplicate count or structure has shifted in
  develop relative to what this design analyzed, the implementation
  must re-run the duplicate-group analysis at the start.

## Refactor mechanics

**Per duplicate group (149 total):**

1. **Determine the anchor `TranslationId`:**
   - If any of the duplicate constructors IS a top-level `TranslationId`
     constructor with literal translations matching the group's locales
     → that's the anchor (38 groups).
   - Otherwise, mint a new top-level `TranslationId` constructor (111
     groups). Validate no clash with existing top-level constructors;
     if the chosen name clashes, skip the group + flag in the follow-up
     doc.

2. **Add the anchor's literal block** (only for the 111 new ones) — to
   the main `translationSet` function, in alphabetical position
   matching the existing rough alphabetical order.

3. **Replace each duplicate literal block** with `translationSet <Anchor>`:

   Before (5-line literal):
   ```elm
       OutcomeOther ->
           { english = "Other"
           , kinyarwanda = Just "Ibindi"
           , kirundi = Just "Ibindi"
           , somali = Just "Kale"
           }
   ```

   After (1-line dispatch):
   ```elm
       OutcomeOther ->
           translationSet Other
   ```

**Naming convention for new anchors (111 groups):**

- PascalCase of the english label, with spaces removed and
  non-alphanumeric chars stripped.
  - `"Other"` → `Other`
  - `"None of the Above"` → `NoneOfTheAbove`
  - `"Hepatitis B"` → `HepatitisB`
  - `"Methyldopa (250mg)"` → `Methyldopa250mg` (parentheses stripped)
- Edge cases that get skipped + flagged:
  - Constructor name would exceed 50 chars (rare but possible for very
    long english phrases like "1 tablet by mouth twice a day").
  - Constructor name would start with a digit (e.g., `"3 Days"` →
    `ThreeDays` — converting digit-words is fragile; safer to skip).
  - Constructor name clashes with an existing top-level
    `TranslationId` constructor.
- Anchor placement: alphabetical insertion within the `type
  TranslationId` declaration AND within the main `translationSet`
  function's case-of body.

**Source-bug handling (in scope per Q3 = B):**

When the refactor encounters a structural bug:

- **Duplicate case branch within same enum** (e.g.,
  `DiagnosisSimpleColdAndCough` appearing twice with different rw
  translations): web-search the english label, decide canonical
  translation per locale, **delete the second case branch + replace
  the first's translations with the canonical values**. Internet
  research notes documented in the commit message for each bug fix.
- **Obvious typo constructor names** (e.g., `NitritionSigns`): NOT
  auto-fixed. Renaming changes call sites. Document in follow-up.
- **Empty-string english (`""`) groups**: skip from refactor; document.

## Verification

Per refactor commit, three checks:

| Check | Command | Why |
|---|---|---|
| Elm format | `elm-format --validate client/src/elm/Translate.elm` | Per CLAUDE.md and CI's `lint_elm` job |
| Elm compile | `cd client && ./node_modules/.bin/elm make src/elm/Main.elm --output=/dev/null` | Confirms file still typechecks; new constructors don't break the type definition |
| Cross-check translations | Python script comparing (english, kw, kn, so) tuples per refactored constructor before vs after — must be byte-identical | Critical semantic check; catches accidental translation drift |

The CI on the eventual PR runs the full suite (elm-test, elm-review,
SimpleTest); we don't disable any of those. We just don't gate local
commits on them.

## Repository changes

1. **Modify `client/src/elm/Translate.elm`** — major changes:
   - Add 111 new top-level `TranslationId` constructors (in the `type
     TranslationId = ...` declaration).
   - Add 111 new case branches in the main `translationSet` function
     (for the new anchors' literal translations).
   - Replace ~370 duplicate literal blocks with `translationSet
     <Anchor>` dispatches.
   - Fix ~5–15 source bugs (delete duplicate case branches; canonicalize
     translations).
   - Net line delta estimate: −1500 to −2000 lines (each 5-line literal
     block collapses to 1 line, minus the 111 added 5-line anchor
     blocks).

2. **Create
   `docs/superpowers/follow-ups-translate-elm-bugs.md`** — listing:
   - The 126 diverging groups (with samples of the locale divergence
     so reviewers know what they're looking at).
   - Skipped clash cases from the 111 (where the proposed anchor name
     collided with an existing constructor).
   - Skipped overlong / digit-leading anchor cases.
   - Constructor-rename candidates (typos like `NitritionSigns`).
   - Empty-string english groups (intentional placeholders).

No other files touched.

## Commits

5 commits on the `refactor/translate-elm-dedup` branch:

| # | Commit | Touches | `[ci skip]`? |
|---|---|---|---|
| 1 | Spec + plan combined | `docs/superpowers/{specs,plans}/2026-04-20-translate-elm-dedup-*.md` | yes (docs only) |
| 2 | Add 111 new top-level `TranslationId` anchors + their case branches | `Translate.elm` | **no** (CI verification critical) |
| 3 | Refactor 149 groups → `translationSet X` calls | `Translate.elm` | **no** |
| 4 | Source-bug fixes (one commit per logical bug, with internet-research notes) | `Translate.elm` | **no** |
| 5 | Follow-up doc for diverging groups + skipped cases | `docs/superpowers/follow-ups-translate-elm-bugs.md` | yes (docs only) |

Each non-`[ci skip]` commit includes a `Co-Authored-By: Claude Opus
4.7` line.

**Push** when all 5 commits are clean and verification passes; PR
title and body draft will be presented for approval before opening.

## Sequencing for the implementation plan

Full detail in writing-plans next:

1. Confirm clean working tree on `refactor/translate-elm-dedup` (already
   set up).
2. Re-run the duplicate-group analysis script against the current
   `Translate.elm` to confirm the 149 / 111 / 38 / 126 / source-bug
   counts haven't shifted in `develop` since this design's
   exploration.
3. Commit spec + plan (Commit 1).
4. Add the 111 new top-level anchors (Commit 2). Run all 3
   verification checks.
5. Refactor the 149 groups (Commit 3). Run all 3 verification checks
   including cross-check semantic equivalence.
6. Fix source bugs one at a time (Commits 4a, 4b, …) with internet
   research notes per fix. Each commit runs all 3 verification checks.
7. Write the follow-up doc (Commit 5).
8. Final verification — full local check suite passes, net line delta
   in expected range, top-level `TranslationId` constructor count
   increased by exactly 111 (modulo any clash skips).
9. Push branch + draft PR.
