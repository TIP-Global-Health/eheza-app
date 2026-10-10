---
name: design-brief-module-splits
description: "Design brief (parked R2-#5 remainder) — splitting the god modules (Translate.elm 30.8k, Measurement/Utils.elm 10.4k, Backend/Update.elm 10.4k) with zero behavior change. Facts verified on develop @91fb401fb, 2026-07-06."
metadata: 
  node_type: memory
  type: project
  originSessionId: 621f6a16-0139-4571-9899-6d1a21bb15f8
---

# Design brief — God-module splits (parked ledger #5 remainder)

**Status:** brief only, not approved. The save-handler dedup half of #5 was done (PR #1772); this covers the module-splitting half. Facts verified on develop @91fb401fb (2026-07-06).

## Verified current state

| File | Lines | Structure |
|---|---|---|
| Translate.elm | 30,804 | one `TranslationId` union, ~1,935 constructors; `translationSet` = ONE case over all of them; 4 satellite unions already nested as payloads (`LoginPhrase`, `ChartPhrase`, `Dashboard`, `ValidationError`) — the extraction pattern already exists in-file; `Translate/Model.elm` + `Translate/Utils.elm` embryos exist |
| Measurement/Utils.elm | 10,440 | 323 top-level fns in clear name-clusters: 52 `*FormWithDefault` + 48 `to*ValueWithDefault` (paired), 73 `*Test*` (labs), 36 vaccination/immunisation, 22 `view*`, misc generate/resolve/expect |
| Backend/Update.elm | 10,405 | `updateIndexedDb` = one case over 224 MsgIndexedDb constructors (~5,030 lines) + `handleRevision` = one case over 214 Revision constructors (~3,490 lines) + ~25 helpers (~1,700 lines); **17 `Backend/*/Update.elm` sub-update delegation modules already exist** (the idiom to generalize) |
| Pages/Prenatal/Activity/Utils.elm | 7,105 | next candidate after the big three |

elm-review constraints: `NoUnused.Exports` active (cross-module use counts — split-sibling exposure is fine; "for future use" exposure gets flagged); `NoMissingTypeAnnotation` + `Simplify` have per-file waivers for Measurement/Utils.elm that must carry to (or be fixed in) split-out files; `NoImportingEverything` allowlist exists.

## Why bother (and why maybe not)

FOR: editor/elm-language-server responsiveness on 10-30k-line files; merge-conflict surface (every i18n PR touches Translate.elm; every backend feature touches Backend/Update.elm); reviewability; incremental-compile granularity. AGAINST: pure churn — conflicts with any open PRs during the split; git-blame history fragmentation; zero user-facing value. **Timing rule: land each split when no feature PRs are open against the file.**

## Design — three independent tracks, each mechanical + compile-driven

**Track 1 — Backend/Update.elm (S, ~1 day, lowest risk, do first).**
Extract `handleRevision` + the `generate*Msgs`/`makeEditableSession`/`summarizeBy*` helpers into `Backend/UpdateRevisions.elm` (or Backend/Revisions.elm) — no type changes, no dispatch changes; Update.elm drops to ~5k. VERIFY at impl: handleRevision must not call back into updateIndexedDb (import cycle) — if it emits msgs only, it's clean. The 224-arm main case stays (splitting IT would need Msg sub-unions = real type churn; not worth it — the existing 17 sub-update modules already delegate the per-entity logic).

**Track 2 — Measurement/Utils.elm (M, ~2-3 days).**
Split by the verified clusters into `Measurement/Utils/Labs.elm` (73 fns), `Measurement/Utils/Vaccination.elm` (36), `Measurement/Utils/Forms.elm` (the FormWithDefault/toValueWithDefault pairs — keep each pair together), re-export nothing from the old module (update importers; compile-driven). Carry the review waivers to the new files OR fix annotations while moving (preferred if cheap). Keep `Measurement/Utils.elm` as the residue, not a re-export facade (facades trip NoUnused.Exports and hide the win).

**Track 3 — Translate.elm (M-L, highest payoff, per-domain PRs).**
Use the IN-FILE precedent: nest per-domain unions as payload constructors (like `Dashboard Dashboard` today). Per domain D (Prenatal, WellChild, NCD, Dashboard-extensions, …): create `Translate/D.elm` with `type DPhrase = …` + `translationSetD : DPhrase -> TranslationSet String`; add ONE constructor `TranslateD DPhrase` to TranslationId; move the ~N constructors + their case arms; mechanically rewrite call-sites (`Translate.Foo x` → `Translate.TranslateD (Translate.D.Foo x)`). Exhaustiveness is PRESERVED (each module's case is total over its own union — no wildcard chains, no lost compiler checking). One domain per PR; start with a small domain to validate the recipe, then the biggest clusters. Alphabetical-ordering convention applies to the new unions/cases (project rule).

## What NOT to do

- No `translationSet` chain-split into `Maybe`-returning partial functions — loses exhaustiveness checking, the compiler's guarantee that every id is translated.
- No re-export facades (NoUnused.Exports + hides the size win).
- No Msg-union splitting in Backend/Update.elm (type churn across the app for structure-only benefit).

## Effort / sequencing

Track 1 (1d) → Track 2 (2-3d) → Track 3 (per-domain, ~0.5-1d each, stop anytime). Each PR: full elm make + elm-test + elm-review + elm-format; behavior-identical by construction (moves only), so no bespoke tests — the compiler is the test.

## Open decisions (user)

1. Is the churn acceptable at all, and when (needs a quiet window per file)?
2. Track 3 domain order (suggest: smallest first as recipe validation, then Prenatal — the largest cluster)?
3. Fix annotations while moving Measurement/Utils clusters, or carry waivers?
