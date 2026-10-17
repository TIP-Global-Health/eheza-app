---
name: ocl-pih-mappings
description: PR mapping E-Heza measurements to PIH OCL dictionary; entry point and operational gotchas
metadata:
  node_type: memory
  type: project
  originSessionId: 010b6507-bb7c-4dc8-8374-4868cc6f5d5d
---

The branch `docs/ocl-pih-mappings` publishes E-Heza's clinical measurements to OCL (Open Concept Lab) under `TIP-Global-Health/EHEZA`, with `SAME-AS` mappings to `PIH/PIH`. All artefacts live under `docs/ocl/` — review input only, nothing compiled or loaded at runtime.

**Why:** External dictionary publishing for interoperability with Partners In Health. No concept codes are referenced from Elm or Drupal yet — mapping is external-only for now.

**How to apply:** `docs/ocl/README.md` is the canonical entry point — read it first for schema, methodology, verification commands, and upload workflow. Coverage as of last commit: 207 concepts/mappings across Prenatal, Nutrition, Well-Child, Acute Illness, NCD, HIV, Tuberculosis, Child Scoreboard, Home Visit. Family Nutrition and group/education/stock-management flows deliberately deferred. `InmmunizationEncounter` intentionally skipped (marked deprecated in code).

Non-obvious operational facts (easy to get wrong):

1. **Always upload via `delta_upload.py`, never the full set.** Re-submitting unchanged rows makes OCL mint a fresh concept-version per row, cluttering source-wide search with ghost snapshots. The script diffs CSVs vs OCL HEAD and emits only new/changed rows.
2. **Cutting a released source version (step 4 in README) is not optional.** Bulk-import writes to HEAD only; search and version-pinned URLs surface content only from *released* versions. Skip step 4 and new concepts appear invisible.
3. **Orphans are intentionally not deleted.** `delta_upload.py` reports concepts present on OCL but missing from CSVs — retire/purge them by hand if really wanted, so nothing accidentally vanishes from release history.
4. **Don't auto-fill non-English locale names.** Per CLAUDE.md, a `Nothing` translation in `Translate.elm` falls back to English at runtime. Auto-populating OCL non-English fields with the English string would pollute the dictionary with unverified translations. The shared `translations.jsonl` carries only `Just _` values from `Translate.elm`; native-speaker review is a separate follow-up.
5. **Reuse over duplication.** When a later encounter's enum constructor matches an already-published concept (e.g., `NCDASign.ChildGotDiarrhea` reusing `EH-NUT-008` Diarrhea), extend the existing concept's `eheza_field_path` instead of minting a new `EH-` ID. Gaps files document each reuse.
6. Requires `OCL_API_TOKEN` env var. Source path: `/orgs/TIP-Global-Health/sources/EHEZA/HEAD`. PIH source: `/orgs/PIH/sources/PIH/`.
