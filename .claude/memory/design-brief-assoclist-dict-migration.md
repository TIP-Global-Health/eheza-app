---
name: design-brief-assoclist-dict-migration
description: "Design brief (parked R2-#3) — client performance; replace O(n) AssocList/EverySet hot paths with comparable-keyed structures + adopt Html.Lazy. Facts verified on develop @91fb401fb, 2026-07-06."
metadata: 
  node_type: memory
  type: project
  originSessionId: 621f6a16-0139-4571-9899-6d1a21bb15f8
---

# Design brief — AssocList→Dict migration + rendering levers (parked ledger #3 / R2-#3)

**Status:** brief only, not approved. Facts verified on develop @91fb401fb (2026-07-06).

## Verified current state

- `AssocList` is a **VENDORED FORK** at `client/src/elm/AssocList.elm` (elm-review-ignored), NOT the pzp1997 package (that's only an indirect dep via Gizra/elm-all-set). The fork was modified: `insert` appends new keys at the END and replaces in place (upstream prepends + moves-to-head). Its `toList` doc comment ("most recently inserted at head") is stale — code returns oldest-first.
- Scale: 171 files import it; ~2,299 `Dict.` call-sites (get 740, insert 392, fromList 214, toList 128, values 105). All ops O(n); `fromList` is O(n²) (foldl insert).
- `EverySet` (Gizra/elm-all-set 1.0.1) is backed by **upstream** assoc-list → `member` O(n), `fromList` O(n²), and iteration order is NEWEST-first — different from the app dict's oldest-first. 148 files, ~1,617 call-sites.
- Keys: `EntityUuid a = EntityUuid String` (vendored Restful/Endpoint.elm:1154) — phantom-typed String wrappers (~200 aliases in Backend/Entities.elm). A comparable is one `fromEntityUuid` away. BUT some page dicts use tuple keys with UNION components (`( PrenatalEncounterId, PrenatalActivity )`, App/Model.elm:309) — not String-backed.
- Order reliance: of 128 `Dict.toList` sites only 11 sort afterwards. Known order-load-bearing: `Backend/NutritionEncounter/Utils.elm:350-360` `getParticipantEncountersByEncounterType` (raw insertion order, backs 9 per-program wrappers); `Pages/Activity/Update.elm:84/90/132` (`toList |> head`).
- Hot growth: `Backend/Model.elm` ~90 Dict fields (people :133 grown via `Dict.union`; peopleInVillage; all *Encounters/*Measurements) + ~45 per-page dicts in App/Model.elm — no pruning anywhere (R4-4 parked separately).
- Rendering: **Html.Lazy 0 uses; Html.Keyed 1 use** (Gizra/Html.elm helpers, 4 files). ZScore uses its own Utils/AllDict (ord-function dict) — an in-repo precedent for comparable-avoiding trees.

## Goal / non-goals

- GOAL: reduce UI latency on large-HC devices (search, dashboards, participant lists) with bounded, verifiable steps.
- NON-GOALS: page-dict eviction (R4-4, parked separately); big-bang replacement of all 2,299 sites.

## Design — four independent levers, cheapest-first

**Phase 0 — MEASURE (mandatory gate; ~0.5 day).** No field complaint is on record; the "biggest perf lever" claim is structural, not measured. Instrument: temporary console.time around `update`/`view` on a demo DB sized like a big HC (thousands of persons); capture search-as-you-type latency, dashboard render, participant-list render. Decision gate: if p95 interaction < ~100ms on representative hardware, STOP — park again with numbers.

**Phase 1 — Html.Lazy adoption (S effort, zero data-structure risk).** The app renders every visible list item through full view functions on every Msg (50ms `CheckDataWanted` tick included). `Html.lazy`/`lazy2` on stable-argument sub-views (person list rows, dashboard cards, activity task lists) is the classic Elm win and is completely untouched today. Requires reference-stable arguments — audit that candidate sub-view inputs aren't rebuilt each pass (records rebuilt inline defeat lazy). Add Html.Keyed to the big lists while there.

**Phase 2 — kill the O(n²) builds in the vendored fork (S).** Because the fork is OURS: optimize `fromList` (build the list once, dedup last-wins via a Set of seen keys — O(n·log n) with a comparable extract unavailable generically, but O(n) dedup possible with `List.foldr` + membership on a small accumulating list… realistically: special-case bulk constructors at CALL sites that build from server lists, e.g. `Backend/Update.elm:1582` people ingestion via `Dict.union`). Also `Dict.union` of fetch results is O(n·m) — ingestion of a 500-row batch into a 5k dict = 2.5M ops; a fork-internal `unionAppend` that concats + dedups once is O(n+m) with the same order semantics. Verify semantics with fork-local unit tests (order preserved, last-wins).

**Phase 3 — UuidDict for the hottest Backend.Model fields (M).** New module `UuidDict k v` wrapping `elm/core Dict String v` + phantom-typed API (`get : EntityUuid a -> UuidDict (EntityUuid a) v`), keys unwrapped via `fromEntityUuid` internally. Migrate ONLY measured-hot, EntityUuid-keyed fields (candidates: `people`, `peopleInVillage`, `individualParticipantsByPerson`, per-program `*EncountersByParticipant`). NOT the tuple/union-keyed page dicts (they'd need key serializers; leave as AssocList). ORDER AUDIT per field before migrating: elm/core Dict iterates key-sorted (UUID-lexicographic ≈ random) — any consumer reading raw `toList`/`values` order from a migrated field must first be given explicit sorting (the `getParticipantEncountersByEncounterType` family is the known trap — it must sort by startDate explicitly BEFORE its backing dict migrates).

**Phase 4 — EverySet hot membership (M, optional).** For measured-hot sets of comparable-representable values, swap to `Set String`/wrapper. Note EverySet's newest-first iteration is user-visible in some sign lists — same order-audit discipline.

## Risks / mitigations

- Silent UI ordering changes (the #1 risk): per-field order audit + explicit sorts landed as separate no-op PRs BEFORE the data-structure swap.
- Lazy that never fires (rebuilt args): verify with Debug.log-in-dev or elm-program-test-style checks during development only.
- Fork divergence: any fork-internal change needs unit tests in-repo (the fork is elm-review-ignored — tests are the only guard).
- Type churn in Phase 3 signatures: contained by making UuidDict's API AssocList-compatible (same function names/argument order), so migration per field is mostly import+type-annotation changes.

## Effort summary

Phase 0: 0.5d · Phase 1: 1–2d · Phase 2: 1d · Phase 3: 2–4d (per-field, incremental PRs) · Phase 4: 1–2d. Each phase independently shippable; stop at any gate.

## Open decisions (user)

1. Approve Phase 0 measurement first? (Everything else is gated on its numbers.)
2. Device/dataset to measure on (real tablet + big-HC demo data?).
3. Appetite for Phase 3 type churn vs stopping at Phases 1–2.
