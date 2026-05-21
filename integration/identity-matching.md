# E-Heza Person → OpenMRS — identity matching

The OpenFN step that decides whether an incoming E-Heza person already
exists in OpenMRS, so the load step links to the existing patient instead
of creating a duplicate.

Place in the pipeline:

```
E-Heza webhook → transform (#12) → match (#13) → load (#14)
```

## Input / output contract

The step receives, from the transform:

| State key | Source | Used for |
|---|---|---|
| `state.data` | E-Heza Drupal person payload | matching keys (national ID, names, birth date) |
| `state.openmrsPatient` | transform output | passed through untouched, for the load step |
| `state.configuration` | OpenFN credential | OpenMRS base URL + integration-user Basic auth |

It adds one key and leaves `data` and `openmrsPatient` intact:

```js
state.match = {
  action: 'link' | 'create',   // link an existing patient, or create a new one
  patientUuid: <uuid> | null,  // the matched OpenMRS patient, when action is 'link'
  via: 'national-id' | 'name-birthdate' | 'none',  // how the decision was reached
  candidates: [                // every patient weighed — the audit trail
    { uuid, display, birthdate },
  ],
};
```

An **ambiguous** result produces no `state.match` — the step throws instead
(see below).

## Matching algorithm

Two tiers, exact comparisons only — no fuzzy scoring.

### Tier 1 — national ID

Run only when `data.national_id` is present.

`GET /patient?identifier={national_id}` (representation including
`person.birthdate`).

| Hits | Outcome |
|---|---|
| 1 | **link** — `via: 'national-id'` |
| >1 | **ambiguous** — national IDs are meant to be unique; more than one is a data problem for a human |
| 0 | fall through to Tier 2 |

### Tier 2 — name + birth date

`GET /patient?q={first_name} {second_name}`, then keep only candidates
whose `person.birthdate` (date part) equals `data.birth_date` (date part).

| Candidates after the birth-date filter | Outcome |
|---|---|
| 0 | **create** — `via: 'none'` |
| 1 | **link** — `via: 'name-birthdate'` |
| >1 | **ambiguous** |

### Ambiguous handling

The step throws, so the OpenFN run fails and surfaces in Lightning for
manual review. No patient is created and no link is made. Once a human
resolves the duplication in OpenMRS, the run can be retried.

### Edge cases

- **No `birth_date`** — Tier 2 cannot filter safely. Any name hit is then
  treated as ambiguous (a human confirms); no name hits → create.
- **No `national_id` and no `birth_date`** — Tier 1 is skipped and Tier 2
  degrades as above. (Real E-Heza registrations carry a birth date; this
  guards the rare incomplete record.)
- **Missing name** — cannot search; the step throws. E-Heza requires both
  name parts, so this indicates a malformed payload.

## Structure and testing

The job does HTTP, so its operation is not a pure function. To keep the
decision logic testable the file is split:

- **Pure decision functions** — one per tier, taking the incoming person
  and the raw search results and returning a decision (or throwing for an
  ambiguous result). No network. Unit-tested exhaustively with `node --test`
  against constructed candidate sets, the way `transform.js` is tested.
- **Thin OpenFN glue** — issues the one or two `GET`s and feeds the pure
  functions. Tier 2 runs only when Tier 1 does not decide, so a clear
  national-ID match costs a single request.

A live smoke test against the local OpenMRS instance covers the glue.

## Configuration

- **Adaptor:** `@openfn/language-http` — OpenMRS REST is plain HTTP + JSON.
- **Auth:** Basic auth as the OpenMRS integration user; the base URL and
  credentials come from an OpenFN credential (`state.configuration`), never
  committed. Local PoC values mirror `integration/openmrs/openmrs-metadata.json`.

## Audit

Every decision is `console.log`'d as a structured line — action, `via`,
matched patient UUID, and the candidates considered — landing in the
Lightning run log. Sufficient for the PoC; a durable audit store is a
production concern.
