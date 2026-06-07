/**
 * Match - decide whether to create or replace the OpenMRS encounter.
 *
 * The encounter attaches to a patient that the Phase 1 person flow already
 * linked, so there is no fuzzy matching here: the patient UUID arrives on
 * the payload. We only decide create-vs-replace (upsert).
 *
 * Input  (state.data.person_openmrs_uuid): linked patient UUID.
 *        (state.data.existing_encounter_uuid): set if already pushed.
 * Output (state.encounterMatch): { action: 'create'|'replace', patientUuid,
 *        previousEncounterUuid }.
 *
 * Adaptor: @openfn/language-common
 */

fn((state) => {
  const data = state.data || {};
  const patientUuid = data.person_openmrs_uuid;
  if (!patientUuid) {
    throw new Error('Cannot create encounter: person not linked to OpenMRS.');
  }

  const existing = data.existing_encounter_uuid;
  if (existing) {
    return {
      ...state,
      encounterMatch: {
        action: 'replace',
        patientUuid,
        previousEncounterUuid: existing,
      },
    };
  }

  return {
    ...state,
    encounterMatch: { action: 'create', patientUuid },
  };
});
