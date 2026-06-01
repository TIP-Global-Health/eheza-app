/**
 * Load step — create the OpenMRS encounter and record the result back on the
 * E-Heza encounter. See integration/load-step.md. Create-once for the PoC.
 *
 * Input  (state.encounterMatch): { action: 'create' | 'skip', patientUuid,
 *        encounterUuid } from match-encounter.
 *        (state.openmrsEncounter): the transform output — the POST body.
 *        (state.data.encounter_uuid): the E-Heza encounter UUID.
 *        (state.configuration): OpenMRS base URL, the E-Heza encounter-link
 *        write-back URL + shared secret.
 * Output (state.loadResult): { action: 'created' | 'skipped', openmrsUuid }.
 *        On 'skip' no HTTP is done and the already-linked UUID is echoed back.
 *
 * Adaptor: @openfn/language-http
 */

fn(async (state) => {
  const match = state.encounterMatch || {};
  const cfg = state.configuration || {};

  if (match.action === 'skip') {
    console.log('Loaded encounter -> skipped', match.encounterUuid);
    return {
      ...state,
      loadResult: { action: 'skipped', openmrsUuid: match.encounterUuid },
    };
  }

  // Create the OpenMRS encounter.
  const res = await post(cfg.openmrsBaseUrl + '/encounter', state.openmrsEncounter, {
    headers: { Authorization: cfg.openmrsAuth },
  })(state);
  const openmrsUuid = res.data && res.data.uuid;
  if (!openmrsUuid) {
    throw new Error('OpenMRS encounter create returned no uuid');
  }

  // Record the OpenMRS encounter UUID on the E-Heza encounter.
  await post(
    cfg.ehezaEncounterLinkUrl,
    {
      encounter_uuid: state.data && state.data.encounter_uuid,
      openmrs_uuid: openmrsUuid,
    },
    { headers: { 'X-OpenFN-Token': cfg.ehezaToken } }
  )(state);

  console.log('Loaded encounter -> created', openmrsUuid);

  return {
    ...state,
    loadResult: { action: 'created', openmrsUuid },
  };
});
