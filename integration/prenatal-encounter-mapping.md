# E-Heza prenatal encounter → OpenMRS encounter — field mapping

One E-Heza prenatal encounter becomes one OpenMRS `Encounter` attached to
the already-linked patient, with one `obs` per measurement field. There is
no OCL/UVL dictionary dependency: each field maps to a **self-provisioned**
OpenMRS Concept, listed in `integration/openmrs/prenatal-concept-catalog.json`
and created by `integration/openmrs/provision.py`. Concept UUIDs live in
`integration/openmrs/openmrs-metadata.json` (placeholder locally; UVL's real
UUIDs when the target switches).

## Pipeline

1. Drupal `hedley_openmrs` queues the encounter on save and POSTs a flat
   payload (`hedley_openmrs_build_prenatal_payload`) to the OpenFN webhook.
2. `transform-encounter.js` loops the catalog → OpenMRS encounter + obs.
   Only fields that are in the catalog **and** carry a non-empty value emit
   an obs; everything else is dropped.
3. `match-encounter.js` resolves the patient. The patient UUID arrives on the
   payload (the Phase 1 person flow already linked it), so there is no fuzzy
   matching — only create-vs-skip (create-once for the PoC).
4. `load-encounter.js` POSTs the encounter to OpenMRS and writes the resulting
   encounter UUID back to E-Heza via `openmrs/encounter-link`.

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

## Deferred (PoC scope)

- Re-sync / update of an already-created encounter (void-and-recreate).
- True coded observations with answer concepts (OCL/UVL's call).
- Obs groups / nested structures (flattened to prefixed keys instead).
