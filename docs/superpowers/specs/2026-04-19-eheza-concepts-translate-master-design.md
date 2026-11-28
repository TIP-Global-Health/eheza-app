# E-Heza concepts translate master — design

Builds a second canonical inventory complementing the existing structural
master (`docs/ocl/eheza-concepts-master.csv`). Source: `client/src/elm/Translate.elm`,
walking the `type TranslationId` declaration. Purpose: catalogue the
user-facing labels — including derived clinical concepts (Gravida, BMI,
ApgarScore, etc.) that the structural Backend walk can't see because they
exist only as UI labels, not as Elm record fields or union constructors.

The deliverable is one new CSV (`docs/ocl/eheza-concepts-translate.csv`),
one new methodology doc, and a README section. Pure local artefacts; no
OCL upload, no edits to existing per-encounter PIH-mapped CSVs or to the
structural master.

A subsequent comparison-and-decision pass (separate brainstorm) will diff
the two masters against each other and recommend whether to use one, the
other, or combine.

## Scope

**Source:** `client/src/elm/Translate.elm`, the `type TranslationId`
declaration block (lines 329–2192 of source). 1860 top-level constructor
variants.

**Inclusion rule:** apply a refined heuristic filter (Section §Walk
methodology) that drops obvious UI noise while keeping clinical labels —
including borderline cases like `*Label`-suffix constructors that are
usually clinical field labels. Filter is mechanical and reproducible from
the source. No manual clinical triage step — the heuristic IS the filter.

**Estimated rows after filtering:** ~2000–4000 total. Of the 1860
top-level constructors, the heuristic keeps maybe ~600–1000; each
surviving union-arg constructor expands into one header row + N leaf rows
(one per case branch), averaging ~5–15 leaves each.

**ID space:** `EHEZA-T-NNNN` zero-padded to 4 digits, sequentially
assigned in source order (top of `type TranslationId` to bottom) over the
union of header rows and leaf rows. Once committed, IDs are immutable;
future Elm changes append rows with the next free `EHEZA-T-NNNN`.

**Out of scope (this pass):**
- Mappings to PIH or any other org
- Upload to OCL
- Edits to existing `docs/ocl/*` files except a one-section README addition
- Walking *inside* sub-translators (per-value translations are emitted
  for surviving union-arg constructors, but for cases where a
  Translate.elm `case` branch dispatches to yet another translator, only
  the outer level is walked)
- Comparison report against the structural master (separate brainstorm
  per the user's plan — diff and use/combine decision happen later)
- Any clinical triage of the heuristic survivors
- Manual editing of translation strings (Translate.elm is the source of
  truth; we copy verbatim)

## Schema

`docs/ocl/eheza-concepts-translate.csv` — strict CSV (RFC 4180), UTF-8,
6 columns:

| Column | Description | Example |
|---|---|---|
| `id` | `EHEZA-T-NNNN`, zero-padded to 4 digits, sequentially assigned in source order over emitted rows | `EHEZA-T-0001`, `EHEZA-T-2847` |
| `translation_id` | For zero-arg labels and union-arg headers: the Elm `TranslationId` constructor name verbatim. For union *value* (leaf) rows: qualified `<UnionType>.<Value>` (the dot signals leaf-ness) | `Gravida`, `AbdomenCPESign`, `AbdomenCPESign.Hepatomegaly` |
| `english` | Zero-arg + leaf rows: the literal `english = "..."` value from `Translate.elm`. Union-arg header rows: prettified constructor name (mechanical CamelCase split, no clinical interpretation) | `Gravida`, `Abdomen CPE sign`, `Hepatomegaly` |
| `kinyarwanda` | Zero-arg + leaf rows: the `Just "..."` value if present, else `null`. Union-arg header rows: always `null` (translations live on the leaf rows). | `Inda zose watwise`, `null`, `Kubyimba umwijima` |
| `kirundi` | Same rule as `kinyarwanda` | (similar) |
| `somali` | Same rule | (similar) |

**Header / leaf / zero-arg distinction is derivable** without a dedicated
column:
- Header → `translation_id` no dot AND `english` is the prettified header
  text (translations all `null`)
- Leaf → `translation_id` contains `.` (qualified by union type)
- Zero-arg → `translation_id` no dot AND `english` is a literal label
  (with at least one of the locale columns potentially populated)

**Concrete example** — what the union-arg constructor `AbdomenCPESign`
produces (8 rows total: 1 header + 7 leaves):

| id | translation_id | english | kinyarwanda | kirundi | somali |
|---|---|---|---|---|---|
| `EHEZA-T-0NNN` | `AbdomenCPESign` | `Abdomen CPE sign` | `null` | `null` | `null` |
| `EHEZA-T-0NNN+1` | `AbdomenCPESign.Hepatomegaly` | `Hepatomegaly` | `Kubyimba umwijima` | `Ivyimba ry'igitigu` | `null` |
| `EHEZA-T-0NNN+2` | `AbdomenCPESign.Splenomegaly` | `Splenomegaly` | `Kubyimba urwangashya` | `Ingwara y'indugwe` | `Xameeti Barar` |
| `EHEZA-T-0NNN+3` | `AbdomenCPESign.TPRightUpper` | `Tender to Palpation right upper` | (kw value) | (rn value) | (so value) |
| ... | (4 more leaves: TPRightLower, TPLeftUpper, TPLeftLower, Hernia) | ... | ... | ... | ... |

**Columns intentionally NOT in the schema:**
- `concept_class`, `description`, `eheza_field_path`, `confidence`,
  `source_module` — same rationale as the structural master (premature
  clinical-judgment work; this is an inventory, not a curated dictionary)
- `takes_arg` — derivable from the dot-presence in `translation_id`
- `dispatched_to` — mostly redundant with `translation_id` (most
  union-arg constructors share their name with the union type they
  dispatch on; the rare divergence cases are recoverable from source)

## Walk methodology

Mechanical and deterministic — given the same `Translate.elm` content
and the same heuristic ruleset, the walk produces the same rows in the
same order, so re-running by hand later yields stable IDs.

**Walk order:**
1. Read `client/src/elm/Translate.elm`.
2. Locate the `type TranslationId` block (lines 329–2192 in current
   source — find by regex on `^type TranslationId` and the next
   lowercase-starting top-level declaration).
3. Walk top-level constructors in source order (top → bottom).

**Per top-level constructor — emit decision:**
- **Zero-arg label** (e.g., `Gravida`) → 1 row.
- **Special-arg constructor** (takes `Int`, `(Maybe NominalDate)`,
  record types — 5 of these in current source: `FormError`, `MemoryQuota`,
  `NCDANumberImmunizationAppointmentLabel`, `ReportCompleted`,
  `StorageQuota`) → 1 row using prettified constructor name as english,
  other locales `null`. (Even though most of these are UI noise, they're
  small enough in count to be processed by the same heuristic; most will
  be dropped.)
- **Union-arg constructor** (e.g., `AbdomenCPESign AbdomenCPESign`) →
  1 header row (prettified constructor name as english, other locales
  `null`) + N leaf rows. Leaves come from the inline `case option of` block
  later in `Translate.elm` (e.g., line 2202 has the
  `AbdomenCPESign option -> case option of ...` block); each case branch
  produces one leaf row with the actual `english`/`kinyarwanda`/`kirundi`/
  `somali` from that branch's `TranslationSet`.

**Heuristic filter rules:**

*Drop by suffix:*
- `*Title` (page/section titles)
- `*Helper` (input help text)
- `*Help` (general help text)
- `*Page` (navigation labels)
- `*Button` (UI controls)
- `*Tab` (tab navigation)

*Drop by prefix:*
- `Add*`
- `Click*`
- `Save*`
- `Submit*`
- `Cancel*`
- `Edit*`
- `Delete*`
- `Loading*`
- `Show*`
- `Hide*`
- `Remove*`

*Drop by exact name (low-content UI atoms):*
- `Accept`, `Actions`, `Activities`, `ActionsTaken`, `ActionsToTake`,
  `Yes`, `No`, `OK`, `Done`, `Continue`, `Back`, `Next`, `Previous`,
  `Close`, `Open`, `Send`, `Receive`, `Loading`, `Saving`, `Error`,
  `Success`

*Drop union-arg constructors that wrap a UI-only union type:*
- A union type is "UI-only" if it does NOT appear in the structural
  master (`docs/ocl/eheza-concepts-master.csv`'s `eheza_field_path`
  column as a `union_type` row).
- Examples: `Page`, `Activity`, `LoginPhrase`, `Dashboard` — these are
  UI/dashboard internals not present in any walked `Backend/*/Model.elm`.
- Detection: read the structural master once at walk time, build a set
  of all `union_type` field paths, drop any union-arg TranslationId
  whose argument type isn't in that set.

*Explicit KEEP exceptions* (override the suffix drops above):
- `*Label` suffix → KEEP. Usually clinical field labels (`GenderLabel`,
  `HIVStatusLabel`, `MaritalStatusLabel`, `ModeOfDeliveryLabel`,
  `LevelOfEducationLabel`).
- `*Question` suffix → KEEP. Often clinical questions
  (`AccompanyToFacilityQuestion`, `AcuteIllnessAdverseEventKindsQuestion`).
- `*Warning` suffix → KEEP. Clinical warnings
  (`AcuteIllnessDiagnosisWarning`).

**ID assignment:** sequential, starting at `EHEZA-T-0001`, incrementing by
1 per emitted row (header rows AND leaf rows count). **Once the file is
committed, IDs are immutable.** Future Elm changes append rows with the
next available `EHEZA-T-NNNN` regardless of where they fall in walk
order — the walk-order rule only governs the initial inventory pass.

**Edge cases:**
- **Recursive case-of dispatch** (a value branch dispatching to another
  translator): emit only the outer level. Don't follow nested
  dispatches.
- **Missing translations** (a case branch that doesn't yield a
  `TranslationSet` — rare): skip with a warning printed to stderr.
- **Comments inside case branches**: stripped during parsing.
- **Multi-line translation strings** (using `++` concatenation): emit the
  parser's best-effort concatenation; flag any failures to stderr.

## Build process

One-shot, hand-driven; mirrors the structural master pattern. The walker
is throwaway scratch tooling — never committed.

1. Write `/tmp/eheza-translate-walker.py` (NOT committed) that:
   - Reads `client/src/elm/Translate.elm` and locates the `type TranslationId` block
   - Reads `docs/ocl/eheza-concepts-master.csv` once and builds the in-scope union-type set
   - Applies the heuristic ruleset to each top-level constructor
   - For surviving union-arg constructors, locates the inline `case option of` dispatch block and parses each branch's translations
   - Emits CSV rows in source order with sequential `EHEZA-T-NNNN` ids
   - Per-constructor summary to stderr (kept/dropped, row count emitted)

2. Hand-inspect output for:
   - **False drops** — clinical concepts inadvertently removed by the
     heuristic. Add to KEEP exceptions; re-run.
   - **False keeps** — UI noise that snuck through. Add to drop list;
     re-run.
   - **Translation parsing errors** — garbled `english = "..."`
     extractions or missing case branches.

3. Iterate walker + heuristic rules until output is sane.

4. Update the methodology doc with actual walk date, source SHA, row
   count, and the final tuned heuristic ruleset.

## Validation (before commit)

Six checks plus an optional cross-master sanity check:

| Check | Method | Expected |
|---|---|---|
| No duplicate `id` | `awk -F, 'NR>1 {print $1}' \| sort \| uniq -d` | empty |
| IDs contiguous from `EHEZA-T-0001` | python: parse ids, confirm `nums == range(1, n+1)` | true |
| Every header `translation_id` exists in `Translate.elm` | grep each non-dotted `translation_id` against the source as a constructor | all resolve |
| Every leaf `translation_id` (`<Type>.<Value>`) corresponds to a real case branch | grep each `<Value> ->` inside the matching `<Type> option ->` block | all resolve |
| Every row has non-empty `english` | `awk -F, 'NR>1 && $3==""' \| wc -l` | 0 |
| Row count sanity | `wc -l` minus header | in range 2000–4000 |

**Cross-master sanity** (advisory, not a hard validation): count how
many leaf rows correspond to a `eheza_field_path` in the structural
master. High match rate is expected (the leaves should mostly be union
constructors already in the structural master). High *miss* count
indicates UI-only unions snuck past the filter — tune rules and re-run.

## Repository changes

1. **Create `docs/ocl/eheza-concepts-translate.csv`** — the labels
   inventory.
2. **Create `docs/ocl/eheza-concepts-translate.md`** — methodology doc:
   purpose, schema, walk methodology, heuristic ruleset (final tuned
   version), inventory-pass metadata (date, source SHA, row count),
   process discipline for future Elm changes, explicit
   non-relationship to per-encounter PIH CSVs and the structural master.
3. **Update `docs/ocl/README.md`** — new section after the existing
   *Master inventory* section, introducing the labels-master and
   explaining the parallel-but-separate relationship to the structural
   master.

No `.gitignore` changes. No code changes. No edits to existing
per-encounter CSVs or to the structural master CSV.

## Branch and commits

**Branch:** continue on the existing `docs/eheza-concepts-master`. No new
branch. Inherits the 4 patient-registration commits and the 4 structural-
master commits already on this branch.

**Commit shape:** **a single commit at the end of the process**, ending
with `[ci skip]` per global preference. Controller asks once before that
commit. The commit covers:
- Spec + plan (this file + the writing-plans output)
- Methodology doc (`docs/ocl/eheza-concepts-translate.md`)
- Master CSV (`docs/ocl/eheza-concepts-translate.csv`)
- README update (`docs/ocl/README.md`)

## Sequencing for the implementation plan

Full detail in writing-plans next:
1. Write methodology doc (uncommitted)
2. Write the scratch walker; iterate until output is sane
3. Hand-verify CSV (drop tuning, name tuning) — keep iterating walker +
   heuristic until validation passes
4. Run the 6 validation checks + cross-master sanity check
5. Update methodology doc with actual walk date / source SHA / row count
   / final heuristic ruleset
6. README update
7. Final verification — confirm clean tree minus the 4 new files
8. **Single commit** containing all 4 files — ask user first

## Out of scope (consolidated)

- Comparison report against the structural master (separate brainstorm
  once both files exist; the diff method and use-vs-combine decision
  depend on what the labels master actually contains)
- The use-master-vs-translate-vs-combine decision
- OCL upload
- Mappings to PIH, UVL, or any other org
- Edits to existing per-encounter PIH CSVs or the structural master CSV
- Walking inside sub-translators beyond the immediate case branch level
- Manual clinical triage of heuristic survivors
- Code changes outside `docs/ocl/`
- Reopening the 4 excluded `Backend/*/Model.elm` modules from the
  structural master (`Dashboard`, `ResilienceMessage`, `ResilienceSurvey`,
  `PatientRecord` — these stay excluded for both masters)
