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

## E-Heza side (Drupal `hedley_openmrs`)

Mirrors the Phase 1 person flow:

- **Queue task** `hedley_openmrs_push_prenatal_encounter`, enqueued from
  `hook_node_insert` / `hook_node_update` on `prenatal_encounter` nodes via
  `hedley_general_add_task_to_advanced_queue_by_id`. Advanced-queue retry semantics
  identical to the person task (`retry after` 120, `max attempts` 5).
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
  (shared-secret `X-OpenFN-Token` auth, idempotent, refuses relink to a different
  UUID with `409`) that stores the OpenMRS encounter UUID on a new
  `field_openmrs_encounter_uuid` on the `prenatal_encounter` node.
- **`hedley_update_70xx()`** creates `field_openmrs_encounter_uuid` on existing
  environments (the Pantheon deploy runs `updb`, not `fra`).

## OpenFN side (`integration/openfn/jobs/`)

- **`transform-encounter.js`** — builds
  `{ encounter: { encounterType, patient, encounterDatetime, location,
  encounterProviders, obs[] } }` by looping the catalog over `payload.measurements`,
  emitting `{ concept: <uuid>, value }` per present field. encounterType, location,
  and provider (the integration user) come from job config; encounterDatetime from
  `encounter_date`; patient from `person_openmrs_uuid`. Empty values omitted.
- **`match-encounter.js`** — resolves the OpenMRS patient from
  `person_openmrs_uuid`; if `field_openmrs_encounter_uuid` is already set,
  short-circuits (create-once).
- **`load-encounter.js`** — `POST /ws/rest/v1/encounter`, then calls
  `encounter-link` to write the OpenMRS encounter UUID back to E-Heza.

Each job ships with a test suite, matching the Phase 1 jobs.

## Idempotency / updates (PoC scope)

**Create-once.** If `field_openmrs_encounter_uuid` is already set, the worker skips.
Re-syncing measurements after the encounter has been created is **out of scope** for
the PoC — OpenMRS obs updates require void-and-recreate. Deferred, the same way
Phase 1 deferred photo and native relationships.

## What stays identical to Phase 1

Shared-secret + `X-OpenFN-Token` auth, advanced-queue retry semantics, the
all-UUIDs-from-config rule, and the "local now, swap to UVL later" posture. **No OCL
dependency anywhere.**

## Out of scope / deferred

- Re-sync / update of an already-created encounter (void-and-recreate).
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
- [ ] `match-encounter` job test: resolves patient; short-circuits when already
      linked.
- [ ] `load-encounter` job test: posts the encounter and calls `encounter-link`.
- [ ] `POST /openmrs/encounter-link` with valid token + known encounter UUID →
      `200`, `field_openmrs_encounter_uuid` set; bad token → `401`; unknown
      encounter UUID → `404`; relink to a different UUID → `409`.
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
