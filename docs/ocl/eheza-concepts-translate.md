This file documents `docs/ocl/eheza-concepts-translate.csv` — a labels
inventory built from `client/src/elm/Translate.elm`, walking the
`type TranslationId` block (lines 329–2192 of the current source) and
emitting one row per top-level constructor that survives the heuristic
filter. The purpose is to catalogue user-facing labels including derived
clinical concepts (Gravida, BMI, ApgarScore, …) that the structural
master cannot surface because they exist only as UI labels — not as Elm
record fields or union constructors. The file has its own `EHEZA-T-NNNN`
ID space and is intentionally separate from the structural master and
from the per-encounter PIH-mapped CSVs (no cross-references between any
of them).

## Schema

| Column | Description | Example |
|---|---|---|
| `id` | `EHEZA-T-NNNN`, zero-padded to 4 digits, sequentially assigned in source order over emitted rows | `EHEZA-T-0001`, `EHEZA-T-2847` |
| `translation_id` | For zero-arg labels and union-arg headers: the Elm `TranslationId` constructor name verbatim. For union *value* (leaf) rows: qualified `<UnionType>.<Value>` (the dot signals leaf-ness) | `Gravida`, `AbdomenCPESign`, `AbdomenCPESign.Hepatomegaly` |
| `english` | Zero-arg + leaf rows: the literal `english = "..."` value from `Translate.elm`. Union-arg header rows: prettified constructor name (mechanical CamelCase split, no clinical interpretation) | `Gravida`, `Abdomen CPE sign`, `Hepatomegaly` |
| `kinyarwanda` | Zero-arg + leaf rows: the `Just "..."` value if present, else empty (CSV `null`). Union-arg header rows: always empty. | `Inda zose watwise`, ``, `Kubyimba umwijima` |
| `kirundi` | Same rule | (similar) |
| `somali` | Same rule | (similar) |

Header / leaf / zero-arg rows are derivable from the data without an
explicit column: a *leaf* has a `.` in its `translation_id` (e.g.
`AbdomenCPESign.Hepatomegaly`); a *header* has no dot AND its english
value is the prettified constructor name (locale columns empty); a
*zero-arg label* has no dot AND its english is a literal `Translate.elm`
string (any of the locale columns may be populated).

Concrete example — the `AbdomenCPESign` constructor expands to one
header row plus seven leaf rows (the `NormalAbdomen` branch is dropped
because it dispatches to another `TranslationId` rather than carrying its
own english string):

| id | translation_id | english | kinyarwanda | kirundi | somali |
|---|---|---|---|---|---|
| `EHEZA-T-0002` | `AbdomenCPESign` | `Abdomen cpe sign` | | | |
| `EHEZA-T-0003` | `AbdomenCPESign.Hepatomegaly` | `Hepatomegaly` | `Kubyimba umwijima` | `Ivyimba ry'igitigu` | |
| `EHEZA-T-0004` | `AbdomenCPESign.Splenomegaly` | `Splenomegaly` | `Kubyimba urwangashya` | `Ingwara y'indugwe` | `Xameeti Barar` |
| `EHEZA-T-0005` | `AbdomenCPESign.TPRightUpper` | `Tender to Palpation right upper` | `Igice cyo hejuru iburyo kirababara  iyo ugikanze` | `Bitera bija mu kudidagizwa mu kuryo amasubiza hejuru` | |
| `EHEZA-T-0006` | `AbdomenCPESign.TPRightLower` | `Tender to Palpation right lower` | `Igice cyo hasi iburyo kirababara  iyo ugikanze` | `Bitera bija mu kudidagizwa mu kuryo amasubiza hasi` | |
| `EHEZA-T-0007` | `AbdomenCPESign.TPLeftUpper` | `Tender to Palpation left upper` | `Igice cyo hejuru ibumoso kirababara  iyo ugikanze` | `Bitera bija mu kudidagizwa mu kubamfu amasubiza hejuru` | |
| `EHEZA-T-0008` | `AbdomenCPESign.TPLeftLower` | `Tender to Palpation left lower` | `Igice cyo hasi ibumoso kirababara  iyo ugikanze` | `Bitera bija mu kudidagizwa mu kubamfu amasubiza hasi` | |
| `EHEZA-T-0009` | `AbdomenCPESign.Hernia` | `Hernia` | `Urugingo ruyobera cg rwinjira mu rundi` | `Isoho ry'agace k'umubiri w'indani hama kakavyimbira hanze` | |

The header carries only the prettified english label (acronym fix-ups
applied — see *Walk methodology* below — would render this as
`Abdomen CPE sign`, with `CPE` upcased; the per-row preview above shows
the post-fix value). Locale columns on header rows are intentionally
empty; per-value translations live on the leaf rows.

## Walk methodology

### Walk order

1. Read `client/src/elm/Translate.elm` in full.
2. Locate the `type TranslationId` block — match by regex `^type TranslationId\s*\n` and consume up to the first top-level binding (`^[a-z]\w*\s*[:=]`). In the current source the block runs from line 329 to line 2192.
3. Walk top-level constructors in source order, top → bottom. The walker emits rows in this order, then assigns `EHEZA-T-NNNN` ids contiguously.

### Per-constructor emit decision

- **Zero-argument label** (`Abdomen`, `Gravida`, …) → 1 row. The walker locates the matching `Ctor ->` branch in the inline `translationSet` body and lifts the `english`, `kinyarwanda`, `kirundi`, `somali` field values verbatim (with `Just` unwrapped, `Nothing` rendered as empty cell).
- **Special-arg constructor** wrapping a non-union argument that the walker cannot expand (`Int`, `Maybe NominalDate`, records — there are 5 in the current source: `FormError`, `MemoryQuota`, `NCDANumberImmunizationAppointmentLabel`, `ReportCompleted`, `StorageQuota`) → 1 row, english = prettified constructor name, locale columns empty.
- **Union-arg constructor** whose argument type appears as a `union_type` row in `eheza-concepts-master.csv` (e.g., `AbdomenCPESign AbdomenCPESign`) → 1 header row + N leaf rows. The walker locates the inline `case option of` block under the constructor's branch and lifts each `Value -> { english = ... }` per branch. Branches that dispatch to another `TranslationId` (e.g., `NormalAbdomen -> translationSet Normal`) carry no inline english and are skipped with a `WARN` to stderr.

### Heuristic filter rules

- *Drop by suffix:* `*Title`, `*Helper`, `*Help`, `*Page`, `*Button`, `*Tab`
- *Drop by prefix:* `Add*`, `Click*`, `Save*`, `Submit*`, `Cancel*`, `Edit*`, `Delete*`, `Loading*`, `Show*`, `Hide*`, `Remove*`
- *Drop by exact name:* `Accept`, `Actions`, `Activities`, `ActionsTaken`, `ActionsToTake`, `Yes`, `No`, `OK`, `Done`, `Continue`, `Back`, `Next`, `Previous`, `Close`, `Open`, `Send`, `Receive`, `Loading`, `Saving`, `Error`, `Success`, `EmptyString`
- *Drop union-arg constructors wrapping UI-only union types:* if the arg type is a UnionType not present in `eheza-concepts-master.csv` as a `union_type` row, drop. Examples: `Page`, `Activity`, `LoginPhrase`, `Dashboard`, `Site`, `WarningPopupType`.
- *Explicit KEEP exceptions* (override suffix drops): `*Label` (clinical field labels like `GenderLabel`, `HIVStatusLabel`); `*Question` (clinical questions); `*Warning` (clinical warnings).

### ID assignment

Sequential, starting at `EHEZA-T-0001`, incrementing by 1 per emitted row (header rows AND leaf rows count toward the same sequence). **Once the file is committed, IDs are immutable.** Future additions get the next free `EHEZA-T-NNNN` regardless of where they fall in source order — the walk-order rule only governs the initial inventory pass.

### Acronym fix-ups

After the walker runs, a deterministic post-processing pass rewrites a fixed list of medical / programmatic acronyms in the english column from prettified-CamelCase form (e.g., `Hiv`) back to canonical uppercase (`HIV`). The list lives in the build helper alongside the walker and is applied idempotently — re-running the helper on already-normalised CSV produces the same output. Acronyms covered include `HIV`, `MUAC`, `BMI`, `ANC`, `BP`, `HCT`, `HMIS`, `GPS`, `NCD`, `TB`, `RDT`, `IUD`, `OPV`, `BCG`, `DPT`, `IPV`, `MR`, `PCV`, `NCDA`, `CHW`, `FBF`, `FBFs`, `HC`, `EDD`, `RH`, `HB`, `HbA1c`, `DOT`, `ASAP`, `AST`, `ALT`, `BUN`, `ECD`, `HDL`, `LDL`, `HPV`, `LMP`, `MMS`, `ORS`, `RUTF`, and `CPE`. The fix-up is purely cosmetic — only the english column is touched, locale columns are left untouched, and row count stays identical.

### Edge cases

- **Recursive case-of dispatch** — only the outermost `case option of` block is walked; constructors whose value branches dispatch into a deeper `case` are not recursed into. Each affected branch yields a `WARN` and contributes no leaf row.
- **Branches with no inline english** (e.g., `NormalAbdomen -> translationSet Normal`) — skipped with a stderr warning; no leaf row emitted. Their translations are reachable via the dispatched-to constructor's row.
- **Triple-quoted Elm strings** (`english = """..."""`) — the walker extracts the literal contents verbatim and collapses internal whitespace runs to single spaces so the CSV cell stays one logical line. Without this handling, multi-line english labels (e.g., the long `DeviceNotAuthorized` paragraph and `GroupEncounterUnauthorized2`) would surface as empty cells, and the body parser's "next branch starts at indent + capital letter" heuristic would mis-terminate inside the triple-quoted region.
- **Comments** — both `{- ... -}` block comments and `-- ...` line comments are stripped before parsing so they cannot perturb branch detection.
- **`++`-concatenated multi-line strings** — best-effort: the walker reads the first quoted segment of the english assignment and stops there. Constructors whose english value is built from runtime concatenation tend to be UI scaffolding and were not observed leaking through the heuristic filter in this pass.
- **Constructors with literal `english = ""`** — the only one observed is `EmptyString`, which is intentionally a placeholder rather than a clinical label; it is dropped via the *Drop by exact name* list rather than treated as a parse failure.

## Inventory pass metadata

- **Walk date**: `2026-04-19`
- **Source tree SHA**: `f0765c429c8d3acd39aaa1a4e2b89488d8c704e4`
- **Walker tool**: scratch Python script at `/tmp/eheza-translate-walker.py` (not committed; see *Build process* below)
- **Initial row count**: 2552 (1507 headers + zero-arg labels, 1045 union-value leaves)
- **Heuristic ruleset version**: tuned-from-initial — `EmptyString` was added to the *Drop by exact name* list during the build (its `english` is the literal empty string and the row carries no clinical content); the walker's body parser was also extended to handle Elm `"""..."""` triple-quoted strings so multi-line english labels are extracted rather than dropped.

*Build process is hand-driven and one-shot per the spec; the walker is
scratch tooling and is not preserved in the repo. Re-running the walk
against a future tree requires re-recreating the script from the plan
(or its equivalent), running it from the repo root with `python3
/tmp/eheza-translate-walker.py > out.csv 2> walk.log`, applying the
acronym fix-up helper, and re-validating against the six checks listed
in the original task spec (no duplicate ids, contiguous from
`EHEZA-T-0001`, headers and leaves resolvable in `Translate.elm`,
non-empty english per row, row count in the 2000–4000 sanity band, and
the cross-master sanity check that every leaf's `<UnionType>.<Value>`
appears in `eheza-concepts-master.csv`).*

## Process discipline for future Elm changes

If you add a new `TranslationId` constructor that survives the heuristic filter, append a row to `eheza-concepts-translate.csv` with the next free `EHEZA-T-NNNN` id as part of the same PR. For union-arg constructors, also append leaf rows for each `case` branch that carries an inline english value. The walk-order assignment only governs the initial inventory pass; net-new entries after that get the next free id regardless of where they fall in source order.

## Scope

**In scope** — the `type TranslationId` block in `client/src/elm/Translate.elm` (lines 329–2192 in the current source; 1860 top-level constructors before the heuristic filter, of which 1507 survive and contribute 2552 emitted rows once union-value leaves are included).

**Out of scope** — sub-translator interiors beyond the immediate `case` branch under each `TranslationId` constructor (e.g., the bodies of `translateActivePage`, `translateChartPhrase`, `translateDashboard`, `translateLoginPhrase`, `translateMonth`, `translateHttpError`, …); editing of translation strings (this is a pure inventory pass, not a translation review); OCL upload (the file is not OCL-import-ready — it has no `concept_class` / `description` / `confidence` columns); and any cross-referencing back to the structural master or the per-encounter PIH CSVs.

## Explicit non-relationship to other docs/ocl/ files

This labels inventory has **separate ID spaces, separate purposes, and no cross-references** to either `eheza-concepts-master.csv` (the structural master) or the per-encounter `<x>-concepts.csv` / `<x>-mappings.csv` PIH-mapped pairs. The file is not updated when the structural master gains rows, and is not updated when a per-encounter PIH mapping changes. Future cross-organisation mapping passes choose which master(s) to consume — both can be inputs in parallel, but neither references the other.
