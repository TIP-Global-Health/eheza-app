# OpenMRS Phase 2 — Prenatal encounter → OpenMRS (generic, no OCL)

**Issue:** https://github.com/Gizra/ihangane/issues/3236 (Phase 2)
**Branch:** `openmrs-prenatal-encounter-poc`, based on `openmrs-patient-poc` (Phase 1 / PR #1763)
**Date:** 2026-06-01

## Context

Phase 1 established a one-way E-Heza → OpenFN → OpenMRS **patient** create/update
flow (PR #1763). It transferred registration data *structurally* — onto OpenMRS's
native Patient/Person/Address/Attribute fields — so it needed no concept dictionary
and no OCL mapping. Placeholder OpenMRS metadata UUIDs live in job config and get
swapped for UVL's real UUIDs when the target instance becomes available.

Phase 2 extends the same pipeline to **prenatal encounter** content. We do **not**
have access to UVL's custom OpenMRS instance or its dictionary, and we are
deliberately **not** using the OCL concept mappings here. The goal is to prove the
encounter-transfer mechanism end-to-end against the local PoC OpenMRS, with the same
"local now, config-only swap to UVL later" posture as Phase 1.

### Why an encounter can't be purely structural like a person

OpenMRS stores all clinical encounter data as **Observations**, and every
observation requires a **Concept** reference (`obs.concept_id` is mandatory). Unlike
patient demographics, there is no concept-free native slot for a clinical value like
"hemoglobin = 11.2". So *something* must play the concept role. We sidestep the
OCL/UVL dictionary the same way Phase 1 sidestepped UVL's real metadata: by
**self-provisioning** the needed concepts on the local OpenMRS with placeholder
UUIDs that swap to UVL's later.

## Decisions (settled during brainstorming)

- **Concept strategy:** self-provisioned concept set — one OpenMRS Concept per
  E-Heza prenatal field, created on the local instance, UUIDs in config. Closest
  mirror of how Phase 1 used placeholder metadata.
- **Scope:** *everything* — all ~30 prenatal measurement types / 100+ fields.
- **Coded values:** stored as **Text** observations (the enum value as a string,
  multi-selects joined), not true OpenMRS coded concepts. One concept per field, no
  answer-concept explosion. Matches how Phase 1 stored HIV status / marital status
  as string PersonAttributes.

## Core model

One E-Heza prenatal encounter → **one OpenMRS `Encounter`** attached to the
already-linked patient, carrying **one flat `obs` per measurement field**.

Per-field datatype mapping:

| E-Heza field type | OpenMRS Concept datatype | obs value |
|---|---|---|
| integer / decimal | Numeric | the number |
| text / coded (list) / multi-select | Text | the string (multi-selects joined, e.g. `"fever, headache"`) |
| date / datetime | Date | ISO date |
| boolean | Boolean | true/false |

No obs groups, no answer concepts. Empty values are omitted (same rule as the person
transform).

## The concept catalog (the heart of "everything")

Because we map all ~30 measurement types, the field→concept mapping is **not**
hand-written. A single checked-in catalog file is the source of truth:

`integration/openmrs/prenatal-concept-catalog.json` — one row per field:

```json
{
  "eheza_key": "hemoglobin_count",
  "concept_name": "E-Heza Hemoglobin Count",
  "datatype": "Numeric",
  "measurement": "prenatal_hemoglobin_test"
}
```

- **Generated** programmatically from the E-Heza data model — Drupal field
  definitions (`hedley_*.features.field_base.inc`) give each field's type → OpenMRS
  datatype; the prenatal restful endpoints
  (`hedley_restful/plugins/restful/node/activity/prenatal/*.class.php`) give the JSON
  keys. The generated catalog is **hand-verified once** before commit.
- **`provision.py` (extended)** reads the catalog and idempotently creates each
  Concept on the local OpenMRS (find-by-name then POST, exactly as it does for
  PersonAttributeTypes today), writing the resolved concept UUIDs into
  `openmrs-metadata.json`.
- **The transform** reads the same catalog (UUIDs injected from config) and loops
  it — generic and data-driven, no per-field code.

Sub-structures (e.g. obstetric-history step 2, labs-results that reference other
test nodes) are **flattened into prefixed keys** in the catalog
(`obstetric_history_step2.<field>`) rather than modelled as nested obs.

> **Revision 2026-06-02 — trigger & idempotency.** The original design triggered
> once on `prenatal_encounter` creation and was create-once. That is wrong for the
> real (online) data flow: E-Heza is offline-first with mutable, trickle-synced
> data and **no hard "encounter complete" signal** (the `prenatal_encounter` node
> has no closed/ended field; measurements arrive over ~an hour as *separate* nodes
> that don't re-save the encounter). Triggering on creation captures an empty shell
> and create-once cements it. This revision switches to a **change-driven upsert**:
> trigger on every encounter *and* measurement node save (insert and update), key
> the queue task by encounter so saves coalesce, and make the OpenMRS write an
> **upsert** so the last push wins and the complete encounter eventually lands. The
> sections below reflect the revised design.

## E-Heza side (Drupal `hedley_openmrs`)

Mirrors the Phase 1 person flow:

- **Queue task** `hedley_openmrs_push_prenatal_encounter`, enqueued from
  `hook_node_insert` / `hook_node_update` on **both** the `prenatal_encounter` node
  **and every prenatal measurement node** (any node carrying
  `field_prenatal_encounter`), on **insert and update**. Measurement hooks resolve
  the parent encounter and enqueue keyed by the **encounter** nid, so all of an
  encounter's saves dedup to a single task via
  `hedley_general_add_task_to_advanced_queue_by_id` (which skips a duplicate while a
  task of the same id is `QUEUED`/`PROCESSING`). Advanced-queue retry semantics as
  the person task (`retry after` 120, `max attempts` 5).
  - *Coalescing scope:* dedup holds only while the task is `QUEUED`/`PROCESSING`;
    after a successful run, a later measurement save creates a fresh task and
    re-pushes — which is correct under the upsert model (last push wins). Practical
    effect: roughly one push per processing cycle that saw new data, not one per
    measurement. A time-based "quiet period" debounce to further cut mid-collection
    pushes is a deferred efficiency lever (see Deferred).
- **Payload builder** gathers the encounter node + every measurement node that
  references it, emitting a flat payload:
  ```
  { encounter_uuid, encounter_date, person_uuid, person_openmrs_uuid,
    measurements: { <eheza_key>: value, … } }
  ```
  It walks the measurement nodes generically (reads each field value) rather than
  hand-listing fields.
- **Patient prerequisite:** the payload carries the person's `field_openmrs_uuid`.
  If it is not set yet, the worker returns `FAILURE_RETRY`, so the encounter waits
  for the person to link first — the flow naturally chains off Phase 1.
- **Write-back endpoint** `openmrs/encounter-link` — a clone of `patient-link`
  (shared-secret `X-OpenFN-Token` auth) that stores the OpenMRS encounter UUID on a
  new `field_openmrs_encounter_uuid` on the `prenatal_encounter` node. Under the
  upsert model an upsert legitimately **replaces** the OpenMRS encounter, so the
  endpoint **overwrites** the stored UUID when it changes (logging the change at
  `INFO`) rather than rejecting a relink with `409`. Same UUID is idempotent.
- **`hedley_update_70xx()`** creates `field_openmrs_encounter_uuid` on existing
  environments (the Pantheon deploy runs `updb`, not `fra`).

## OpenFN side (`integration/openfn/jobs/`)

- **`transform-encounter.js`** — builds
  `{ encounter: { encounterType, patient, encounterDatetime, location,
  encounterProviders, obs[] } }` by looping the catalog over `payload.measurements`,
  emitting `{ concept: <uuid>, value }` per present field. encounterType, location,
  and provider (the integration user) come from job config; encounterDatetime from
  `encounter_date`; patient from `person_openmrs_uuid`. Empty values omitted.
- **`match-encounter.js`** — decides **create vs replace** (no fuzzy matching; the
  patient UUID is on the payload). If `existing_encounter_uuid` is set →
  `{ action: 'replace', previousEncounterUuid }`; else `{ action: 'create' }`.
  Throws if the person is not linked.
- **`load-encounter.js`** — upsert. On **replace**, first delete (void) the previous
  OpenMRS encounter (`DELETE /ws/rest/v1/encounter/<previous>`), then create afresh;
  on **create**, just create. Both then `POST /ws/rest/v1/encounter` and call
  `encounter-link` to write the (new) OpenMRS encounter UUID back to E-Heza.
  Delete-and-recreate gives a clean full-snapshot replace (no leftover obs) at the
  cost of a changing encounter UUID — acceptable for the PoC; void-obs-in-place
  (stable UUID) is the production-grade alternative.

Each job ships with a test suite, matching the Phase 1 jobs.

## Idempotency / updates — upsert (revised 2026-06-02)

**Change-driven upsert.** Every encounter/measurement save pushes the current full
snapshot; OpenMRS is made to reflect it via delete-and-recreate:

- First push: `existing_encounter_uuid` empty → `match` says `create` → `load`
  creates the encounter and writes its UUID back.
- Later push (more/edited/deleted measurements): the builder reads the stored UUID
  into `existing_encounter_uuid` → `match` says `replace` → `load` deletes the old
  OpenMRS encounter, creates a fresh one from the latest snapshot, and writes the
  new UUID back (the endpoint overwrites).

Why this resolves the timing problem: the empty/partial encounter created at the
start of a visit is **transient** — each subsequent save overwrites it, so the
final OpenMRS state reflects the complete encounter. Correctness no longer depends
on guessing when "all measurements are in." Soft-deleted measurements drop out of
the next snapshot automatically (the builder already skips `field_deleted`).

## What stays identical to Phase 1

Shared-secret + `X-OpenFN-Token` auth, advanced-queue retry semantics, the
all-UUIDs-from-config rule, and the "local now, swap to UVL later" posture. **No OCL
dependency anywhere.**

## Out of scope / deferred

- **Time-based "quiet period" debounce** — defer the worker until an encounter has
  had no save for N minutes, to cut redundant mid-collection upserts. The upsert
  model is correct without it (last push wins); this is a pure efficiency lever.
  Left out to avoid entangling deferral with the advanced-queue attempt counter.
- **Void-obs-in-place upsert** (stable encounter UUID) instead of delete-and-recreate.
- True OpenMRS coded observations with answer concepts (OCL/UVL's call later).
- Obs groups / nested observation structures.
- Encounter types other than prenatal.
- Reverse sync (OpenMRS → E-Heza).

## Test plan

- [ ] `provision.py` creates one Concept per catalog row on the local OpenMRS and
      writes their UUIDs to `openmrs-metadata.json`; re-running is idempotent.
- [ ] `transform-encounter` job test: a sample encounter payload → an OpenMRS
      encounter body with the expected obs (numeric, text, date, boolean), empty
      fields omitted.
- [ ] `match-encounter` job test: `create` when no existing UUID; `replace` (with
      `previousEncounterUuid`) when set; throws when person not linked.
- [ ] `load-encounter` job test: create path posts + writes back; replace path
      deletes the previous encounter first, then posts + writes back.
- [ ] `POST /openmrs/encounter-link`: valid token + known encounter UUID → `200`,
      `field_openmrs_encounter_uuid` set; same UUID again → `200` (idempotent);
      a **different** UUID → `200` and the stored UUID is **overwritten** (upsert);
      bad token → `401`; unknown encounter UUID → `404`.
- [ ] A measurement-node save (insert or update) enqueues a task keyed by its parent
      encounter; a second measurement save for the same encounter does not add a
      duplicate while the first is still queued.
- [ ] Worker returns `FAILURE_RETRY` when the person is not yet linked.
- [ ] phpcs clean (Drupal + DrupalPractice).
- [ ] `drush updb` on an existing site creates `field_openmrs_encounter_uuid`.

## Risks / notes

- **Catalog generation is ~80% of the work.** Getting the key + datatype right for
  100+ fields across ~30 modules is the bulk of the effort; generating it from the
  field definitions (then verifying) is what makes "everything" tractable.
- Local OpenMRS uses the `-no-demo` reference-application image — concepts must be
  self-provisioned (we don't rely on a bundled CIEL dictionary), which this design
  already assumes.
