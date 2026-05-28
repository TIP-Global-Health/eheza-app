# E-Heza Person → OpenMRS — update flow

Phase 1.5: when an already-synced E-Heza person changes, push the change to
the OpenMRS patient that was created (or linked) by the Phase 1 flow.

Place in the pipeline: same as create — `webhook → transform → match →
load`. The match step picks the branch.

## Trigger — Drupal side

`hedley_openmrs_node_update($node)` on the `person` bundle:

- Build the *relevant-field set* from `hedley_openmrs_build_person_payload`'s
  keys, plus `field_uuid`. The set is closed: photo, reports data, sync
  state, audit timestamps are outside it and never cause an enqueue.
- Diff `$node->original` against `$node` over that set. Short-circuit when
  nothing in the set changed.
- Enqueue the same `hedley_openmrs_push_person` queue task. No new queue.

The payload builder gains one extra key, `existing_openmrs_uuid`,
populated from `field_openmrs_uuid`. It is the only signal the OpenFN side
needs to choose update over create.

## Match — short-circuit on the existing UUID

When `state.data.existing_openmrs_uuid` is set, the match step skips
Tier 1 / Tier 2 and emits:

```js
state.match = {
  action: 'update',
  patientUuid: <existing UUID>,
  via: 'existing-uuid',
  candidates: [],
};
```

Otherwise the existing tier logic runs (a person whose `field_openmrs_uuid`
isn't set yet still goes through national-ID / name+DOB matching).

## Load — the `update` branch

Two locked-in policies make this additive and never destructive:

1. **Empty-source → leave untouched.** A field cleared in E-Heza does not
   void / delete the OpenMRS identifier, attribute, address line, etc.
2. **E-Heza is source of truth.** When values differ we overwrite OpenMRS;
   no diff against OpenMRS, no conflict detection.

### Flow

1. **One GET** to discover OpenMRS subresource UUIDs:

   ```
   GET /patient/{patientUuid}?v=custom:(
     uuid,
     identifiers:(uuid,identifierType:(uuid)),
     person:(
       uuid,
       addresses:(uuid),
       attributes:(uuid,attributeType:(uuid)),
       preferredName:(uuid)
     )
   )
   ```

2. **Person-level fields** — one `POST /person/{personUuid}` with only the
   fields the transform actually emitted, *excluding sentinels* (gender
   `U`, birthdate `null`):

   | Field | Sent when |
   |---|---|
   | `gender` | not `U` |
   | `birthdate` + `birthdateEstimated` | birthdate not null |

3. **Name** — `POST /person/{personUuid}/name/{nameUuid}` with the
   non-empty given/family from the transform; create via `/name` if no
   preferredName exists.

4. **Address** — `POST /person/{personUuid}/address/{addressUuid}` with the
   address fields the transform emitted (already excludes empty values);
   create via `/address` when none exists.

5. **Identifiers** — for each identifier in the desired body (skipping any
   `autoGenerate` placeholder — those make sense only on create):
   - If an OpenMRS identifier of the same `identifierType` exists →
     `POST /patient/{patientUuid}/identifier/{idUuid}`.
   - Else → `POST /patient/{patientUuid}/identifier`.

6. **PersonAttributes** — for each attribute the transform emitted:
   - If an OpenMRS attribute of the same `attributeType` exists →
     `POST /person/{personUuid}/attribute/{attrUuid}`.
   - Else → `POST /person/{personUuid}/attribute`.

7. **No write-back call.** The `field_openmrs_uuid` was set on create and
   does not change on update.

### Result

```js
state.loadResult = { action: 'update', patientUuid, openmrsId: null };
```

## Tests

- Unit (`node:test`) covers the update branch: gender/birthdate sentinel
  skipping, name/address update vs create, identifier upsert by type,
  attribute upsert by type, `autoGenerate` skipped on update, and the
  empty-payload → no-write rule.
- Live smoke: create a patient via Phase 1, then mutate the person in
  E-Heza (change a name, add a phone, clear an attribute), trigger the
  queue, and confirm the OpenMRS person reflects the changes while
  cleared-in-E-Heza fields remain.

## Configuration

Unchanged. Same credential, same workflow, same webhook. The update path
is purely behavioural inside the existing jobs.
