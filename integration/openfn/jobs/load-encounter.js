/**
 * Load step — upsert the OpenMRS encounter and record the result back on the
 * E-Heza encounter. See integration/load-step.md.
 *
 * Upsert model (match-encounter emits 'create' or 'replace', never 'skip'):
 *   - 'create'  (no previous uuid): POST the OpenMRS encounter, then POST the
 *     E-Heza encounter-link write-back.
 *   - 'replace' (encounterMatch.previousEncounterUuid set): FIRST void the
 *     previous OpenMRS encounter (DELETE), THEN create a fresh encounter from
 *     the latest snapshot and write the new UUID back — same create + write-back
 *     steps as 'create'. Void-and-recreate keeps OpenMRS in sync on re-edits.
 *
 * Input  (state.encounterMatch): { action: 'create' | 'replace', patientUuid,
 *        previousEncounterUuid? } from match-encounter.
 *        (state.openmrsEncounter): the transform output — the POST body.
 *        (state.data.encounter_uuid): the E-Heza encounter UUID.
 *        (state.configuration): OpenMRS base URL + auth, the E-Heza
 *        encounter-link write-back URL + shared secret.
 * Output (state.loadResult): { action: 'created' | 'replaced', openmrsUuid }.
 *
 * Adaptor: @openfn/language-http
 */

fn(async (state) => {
  const cfg = state.configuration || {};
  const match = state.encounterMatch || {};

  // Upsert: on replace, void the previous OpenMRS encounter before recreating.
  if (match.action === 'replace' && match.previousEncounterUuid) {
    await del(cfg.openmrsBaseUrl + '/encounter/' + match.previousEncounterUuid, {
      headers: { Authorization: cfg.openmrsAuth },
    })(state);
  }

  // Create the encounter fresh from the latest snapshot.
  const created = await post(cfg.openmrsBaseUrl + '/encounter', state.openmrsEncounter, {
    headers: { Authorization: cfg.openmrsAuth },
  })(state);
  const openmrsUuid = created.data && created.data.uuid;
  if (!openmrsUuid) {
    throw new Error('OpenMRS did not return an encounter uuid.');
  }

  // Write the (new) OpenMRS UUID back to E-Heza (endpoint overwrites on change).
  await post(cfg.ehezaEncounterLinkUrl, {
    encounter_uuid: state.data.encounter_uuid,
    openmrs_uuid: openmrsUuid,
  }, {
    headers: { 'X-OpenFN-Token': cfg.ehezaToken },
  })(state);

  const action = match.action === 'replace' ? 'replaced' : 'created';
  console.log('OpenMRS encounter', action, openmrsUuid);
  return { ...state, loadResult: { action, openmrsUuid } };
});
