# E-Heza Person → OpenMRS — load step

The final OpenFN step: it acts on the matching decision — creating the
OpenMRS patient when needed — and records the patient UUID back on the
E-Heza person.

Place in the pipeline:

```
E-Heza webhook → transform (#12) → match (#13) → load (#14)
```

## Input / output contract

| State key | Source | Used for |
|---|---|---|
| `state.data` | E-Heza Drupal person payload | `person_uuid` for the write-back |
| `state.openmrsPatient` | transform output | the `POST /patient` body |
| `state.match` | match step | `{ action, patientUuid }` — what to do |
| `state.configuration` | OpenFN credential | OpenMRS URL + auth; E-Heza write-back URL + token |

It adds:

```js
state.loadResult = {
  action: 'link' | 'create',
  patientUuid: <uuid>,      // the linked or newly created OpenMRS patient
  openmrsId: <string> | null,  // the generated OpenMRS ID, when one was minted
};
```

## Logic

1. **Resolve the patient UUID.**
   - `match.action === 'link'` → reuse `match.patientUuid`; no OpenMRS write.
   - `match.action === 'create'` →
     - **Resolve identifiers** — replace any identifier the transform flagged
       `autoGenerate` with a freshly minted OpenMRS ID (see below). A
       national-ID identifier passes through untouched.
     - `POST /patient` with the resolved body → the new patient's `uuid`.
2. **Write back to E-Heza** — for both `link` and `create`:
   `POST {ehezaUrl}/openmrs/patient-link` with body
   `{ person_uuid, openmrs_uuid }` and the shared secret in the
   `X-OpenFN-Token` header. The endpoint is the one `hedley_openmrs`
   exposes (#9).

## OpenMRS ID generation

When a person has no national ID, the transform emits an `autoGenerate`
placeholder; the load step mints the value.

- **Format** — `<digits>-<check>`, verified empirically against the local
  instance's `LuhnIdentifierValidator`: a numeric base, a hyphen, then one
  standard-Luhn check digit. (`9876543-1` is accepted; `9876543-2` is not.)
- **Base** — derived deterministically from `data.person_uuid`: the UUID's
  hex reduced modulo 10^12 and zero-padded to 12 digits. The same person
  always yields the same ID, so a re-run is idempotent and two distinct
  persons cannot collide on the base.
- The minted ID goes into the OpenMRS Identification Number identifier;
  its type and location come from the placeholder the transform produced.

For UVL production, OpenMRS ID generation would move to the idgen module;
the local instance has no idgen source configured, hence this approach.

## Error handling

- `POST /patient` fails → the run fails and surfaces in Lightning.
- The write-back fails *after* a successful create → the run fails, but it
  is **self-healing**: the next sync of that person matches the
  now-existing patient (Tier 1 or Tier 2) and links, retrying the
  write-back. No duplicate is created and no link is lost.

## Structure and testing

Pure logic separated from the OpenFN glue, as in the match step:

- **Pure** — `luhnOpenmrsId(personUuid)` and `resolveIdentifiers(patient,
  personUuid)`. Exhaustively unit-tested with `node --test`.
- **Thin glue** — the conditional `POST /patient` and the write-back.

`load.test.js` drives the job through faked `post`/`get`: the link path,
create with a national ID, create with an `autoGenerate` placeholder (the
Luhn ID is resolved into the body), Luhn validity and determinism, the
write-back payload and header. A live smoke test creates a real patient in
OpenMRS and performs a real write-back to the DDEV E-Heza endpoint.

## Configuration

- **Adaptor:** `@openfn/language-http`.
- `state.configuration` carries the OpenMRS base URL and integration-user
  credentials, plus the E-Heza `patient-link` URL and shared secret — all
  from an OpenFN credential, never committed.
