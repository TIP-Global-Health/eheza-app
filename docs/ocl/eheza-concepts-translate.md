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

1. Read `client/src/elm/Translate.elm` in full and strip comments.
2. Locate the `translationSet` function body (matches `^translationSet\s+\w+\s*=\s*\n\s*case\s+\w+\s+of\s*\n` and consumes up to the next top-level binding).
3. Emit one row per `{ english = ..., kinyarwanda = ..., kirundi = ..., somali = ... }` record literal that appears under a top-level case branch (or under an inner case branch one level deep). Rows are assigned `EHEZA-T-NNNN` ids contiguously in source order after the post-walk filter passes.

The walker is **independent of the structural master** — it does not read
`eheza-concepts-master.csv` and its output is determined purely by what
record literals exist in `Translate.elm`. Branches that dispatch (e.g.,
`translationSet OtherLabel` or `translationSet <| OuterCtor InnerCtor`)
carry no record literal and are skipped — no row is emitted.

### Per-record-literal emit decision

- **Top-level zero-arg branch** (`        SomeCtor ->\n            { ... }`) → 1 row with `translation_id = SomeCtor`.
- **Inner case branch under arg-taking outer wrapper**
  (`        OuterCtor x ->\n            case x of\n                InnerCtor ->\n                    { ... }`) → 1 row with `translation_id = OuterCtor.InnerCtor`. Inner branches that themselves take args are recursed one level deeper.
- **Branches without a record literal** (dispatches like `translationSet OtherCtor` or `translationSet <| OuterCtor X`, or arg-taking outers whose body is a complex expression rather than a `case`) — no row emitted.

#### Post-walk phrase / question / instruction filter

After the walker emits rows, a second pass drops entries whose `english`
reads as a UI question, sentence/phrase, or instructional step rather than
a concept label. The filter triggers on three signals:

1. **`translation_id` prefix** — full-tid prefix match against:
   `ResilienceMessage*`, `ResilienceGuideSection*` (staff-coaching message
   bodies), `RecommendedTreatmentSign.Treatment*`,
   `OutsideCareMedication.OutsideCareMedication*`, `MedicationDistributionNotice*`
   (treatment dosing strings), `Recommendation114*` (114-call recommendation
   text), `ErrorCheck*`, `ServiceWorker*`, `ReportToWhatsApp*`
   (system / error / flow message bodies).
2. **`translation_id` suffix** — outer or leaf ends in any of:
   `Inform`, `HelperWellChild`, `Message`, `Instructions`, `Instruction`,
   `Notice`, `Paragraph`, `Paragraph1`, `Paragraph2`, `Question`, `Action`,
   `Phrase`, `Reason`.
3. **English string starter / shape** — sentence-leading word patterns:
   - *Question starters*: `Can`, `Could`, `Do`, `Does`, `Did`, `Are`, `Is`,
     `Has`, `Have`, `Was`, `Were`, `Will`, `Would`, `Should`, `What`,
     `When`, `Where`, `Why`, `How`, `Who`, `Which`, `On which`, `By which`.
   - *Imperative / instructional verbs*: `Give`, `Send`, `Provide`,
     `Advise`, `Advised`, `advised`, `agreed`, `Instruct`, `Inform`,
     `Counsel`, `Refer`, `Encourage`, `Remind`, `Tell`, `Ask`, `Educate`,
     `Reassure`, `Continue`, `Stop`, `Please`, `Use`, `Apply`, `Take`,
     `Avoid`, `Not dispensing`, `Not`, `Choose`, `Enroll`, `Update`,
     `Indicate`, `Check`, `Search`, `Enter`, `Alert`, `Click`, `Select`,
     `Add`, `Remove`, `Submit`, `Save`, `Cancel`, `Confirm`, `Edit`,
     `Review`.
   - *First / second person*: `I`, `My`, `We`, `You`, `Your`,
     `Things have`.
   - *Third-person sentence narratives*: `The thought`, `There are`,
     `The Tetanus`, `The patient`, `The child`, `Prevents`, `Protects`,
     `OPV`, `BCG`, `HPV`, `MR`, `Patient experienced`, `This patient`,
     `This child`, `Patient`, `According to`, `Site recommendation`,
     `A child`, `An adult`, `Children`, `Adults`, `Patients`,
     `All activities`, `All participants`, `No activities`,
     `No participants`, `There is`, `There are`, `This village`,
     `Stabilize`.
   - *Coaching openers*: `If you`, `If`, `Spending`, `Feeling`, `Healthy`,
     `People`, `Research`, `Think`, `Learning`, `At the end`, `In our`,
     `Ever`, `It can`, `Ever been`, `Try`, `Make`.
   - *Confirmation / system prompts*: `Once`, `To proceed`, `To continue`,
     `To enable`, `Before`, `After`, `When you`, `Note that`, `Note:`,
     `At the previous`, `At your previous`.
   - *Dosing instructions*: `1 tablet`, `2 tablet`, `2 capsule`,
     `3 tablet`, `4 tabs`, `4 tablets`, `2 tablets`, `by mouth`,
     `By mouth`, `IM`.
   - *Severity / response*: `Severe Malaria`, `No response`.
   - *Numeric-led recommendations*: `114 ` (114-call recommendation text).
   - *Leading whitespace*: any english starting with a literal space (these
     are sentence-fragment labels designed to be concatenated with another
     value, not standalone concepts).
   - *Multi-sentence text body*: english containing `. ` followed by an
     uppercase letter (sentence boundary inside the string indicates a
     paragraph, not a label).
4. **Explicit tid drop list** — a small hand-curated set of `translation_id`s
   whose english reads as a status sentence but doesn't trigger any of the
   above heuristics: `ThisGroupHasNoMothers`, `FundalPalpableWarning`,
   `NoParticipantsPending`, `NoParticipantsPendingForThisActivity`,
   `PageNotFoundMsg`, `AdoptionSurveyProgressImproving`.

5. **Curated drop list** — `docs/ocl/labels-master-drop-tids.txt` contains
   369 additional `translation_id`s removed by hand-review on 2026-04-20.
   These cover generic UI atoms (`Yes`, `No`, `OK`, `Save`, `Cancel`, etc.),
   lab-range / value enums (`LaboratoryPHValue.*`, `LabResultsNormalRange.*`,
   etc.), workflow / system / state messages (`BeginNewEncounter`,
   `ResultsPending`, `BackendError`, etc.), and Z-score scaffolding rows.
   Future rewalks **must** consult this file and drop any matching tid even
   when it would otherwise survive the heuristics.

Surviving rows are renumbered `EHEZA-T-NNNN` contiguously after the filter
pass.

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

- **Walk date**: `2026-04-20` (independent rewalk; previous version filtered by struct master and missed inner-leaf concepts like `BirthDefect.DefectInguinalHernia`)
- **Walker tool**: scratch Python script at `/tmp/labels-master-rewalk.py` (not committed — see *Build process* below)
- **Walked record literals**: 2970 (every `{ english = ... }` literal under `translationSet`, including inner-case branches one level deep)
- **Empty-english drops**: 2 (`EmptyString`, `EncounterTypePageLabel.PageMain` — both literal empty strings)
- **Translation-id prefix / suffix / exact drops**: 907 (staff-coaching messages, treatment dosing strings, system prompts, etc. — see *Post-walk phrase / question / instruction filter* above)
- **English starter / shape drops**: 355 (questions, imperatives, sentence narratives, multi-sentence paragraphs)
- **Curated hand-review drops** (`labels-master-drop-tids.txt`): 369 (generic UI atoms, lab-range enums, workflow/system messages, Z-score scaffolding — see point 5 of *Heuristic filter rules* above)
- **Manual content corrections during the review pass**: 2 rows (`ActivitiesTitle.MotherActivity` had its english "Forms" replaced with "FBF Mother" plus locale fills; `RecommendedTreatmentSignLabel.TreatmentWrittenProtocols` got a trailing-space fix in its somali column).
- **Final row count**: 1336

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
