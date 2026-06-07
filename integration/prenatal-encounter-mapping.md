# E-Heza prenatal encounter → OpenMRS encounter — field mapping

One E-Heza prenatal encounter becomes one OpenMRS `Encounter` attached to
the already-linked patient, with one `obs` per measurement field. There is
no OCL/UVL dictionary dependency: each field maps to a **self-provisioned**
OpenMRS Concept, listed in `integration/openmrs/prenatal-concept-catalog.json`
and created by `integration/openmrs/provision.py`. Concept UUIDs live in
`integration/openmrs/openmrs-metadata.json` (placeholder locally; UVL's real
UUIDs when the target switches).

## Pipeline

1. Drupal `hedley_openmrs` hooks fire on **insert and update** of both the
   `prenatal_encounter` node and every prenatal measurement node (anything
   carrying `field_prenatal_encounter`). A measurement save resolves its parent
   encounter and enqueues keyed by the **encounter** nid, so all of an
   encounter's saves coalesce into one advanced-queue task (deduped while
   QUEUED/PROCESSING). A loop guard skips the encounter-link write-back's own
   save (which only sets `field_openmrs_encounter_uuid`). The worker POSTs a
   flat payload (`hedley_openmrs_build_prenatal_payload`) of the latest snapshot
   to the OpenFN webhook.
2. `transform-encounter.js` loops the catalog → OpenMRS encounter + obs.
   Only fields that are in the catalog **and** carry a non-empty value emit
   an obs; everything else is dropped.
3. `match-encounter.js` resolves the patient. The patient UUID arrives on the
   payload (the Phase 1 person flow already linked it), so there is no fuzzy
   matching — it only decides **create vs replace** (upsert): `replace` when an
   `existing_encounter_uuid` is already linked, otherwise `create`.
4. `load-encounter.js` upserts the encounter. On `replace` it first voids
   (DELETEs) the previously linked OpenMRS encounter, then on both paths POSTs
   a fresh encounter from the latest snapshot and writes the resulting UUID back
   to E-Heza via `openmrs/encounter-link` (the endpoint overwrites the stored
   UUID on change). Because each push rebuilds from the current snapshot,
   the last push wins, so an encounter that fills in over time eventually lands
   complete; soft-deleted measurements drop out of the next snapshot.

## OpenFN workflow

Wired in `integration/openfn/project.yaml` as the `prenatal-encounter`
workflow, mirroring the Phase 1 `patient-sync` workflow:

| Job | Adaptor | Body |
|---|---|---|
| transform-encounter | `@openfn/language-common` | `jobs/transform-encounter.js` |
| match-encounter | `@openfn/language-common` | `jobs/match-encounter.js` |
| load-encounter | `@openfn/language-http` | `jobs/load-encounter.js` |

Webhook trigger → transform-encounter (`always`) → match-encounter
(`on_job_success`) → load-encounter (`on_job_success`). Connection details
(OpenMRS base URL + auth, the E-Heza encounter-link URL + token) live in
Lightning credentials, not in `project.yaml`.

## Datatype mapping

| Drupal field type | OpenMRS concept datatype | obs value |
|---|---|---|
| number_integer / number_float / number_decimal | Numeric | the number |
| datetime / date | Date | ISO date |
| list_boolean | Boolean | true/false |
| list_text / text / text_long | Text | the string (multi-selects comma-joined as `", "`) |

## Skipped at the source

`hedley_openmrs_build_prenatal_payload` never emits soft-deleted measurements
(`field_deleted`), so voided E-Heza data never reaches OpenMRS.

## Deploy-time verification

`load-encounter.js` voids the previous encounter with the `@openfn/language-http`
`del` operation. Confirm the operation name and behavior against the deployed
adaptor version (`@openfn/language-http@7.3.0`) when wiring the live workflow.
Note that the OpenMRS REST DELETE **voids** (not purges), so a superseded
encounter remains as voided history — acceptable for the PoC.

## Deferred (PoC scope)

- Time-based "quiet period" debounce on the enqueue to reduce mid-collection
  upserts (push once the encounter has stopped changing, rather than on every
  measurement save).
- Void-obs-in-place upsert against a **stable** encounter UUID, instead of the
  current delete-and-recreate (which mints a new UUID and leaves voided history).
- True coded observations with answer concepts (OCL/UVL's call).
- Obs groups / nested structures (flattened to prefixed keys instead).
- Encounter types beyond prenatal.
- Reverse sync (OpenMRS → E-Heza).
