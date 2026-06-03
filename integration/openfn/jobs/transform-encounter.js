/**
 * Transform - E-Heza prenatal-encounter payload -> OpenMRS REST
 * `/encounter` body.
 *
 * Input  (state.data): the encounter payload posted by the E-Heza Drupal
 *        trigger (hedley_openmrs_build_prenatal_payload).
 *        (state.configuration.prenatalConcepts): { eheza_key: conceptUuid }
 *        from openmrs-metadata.json — the generated concept catalog.
 * Output (state.openmrsEncounter): an OpenMRS `POST /ws/rest/v1/encounter`
 *        body with one obs per present, catalogued measurement.
 *
 * Adaptor: @openfn/language-http (no HTTP calls — only fn(); the http adaptor
 * is used so Lightning can attach the credential carrying prenatalConcepts etc.)
 */

fn((state) => {
  const data = state.data || {};
  const cfg = state.configuration || {};
  // prenatalConcepts may arrive as an object or, when supplied via a Lightning
  // credential (which caps sensitive keys at 50), as a single JSON string.
  const concepts =
    typeof cfg.prenatalConcepts === 'string'
      ? JSON.parse(cfg.prenatalConcepts)
      : cfg.prenatalConcepts || {};
  const measurements = data.measurements || {};

  const present = (v) => v !== null && v !== undefined && v !== '';

  const obs = [];
  Object.keys(measurements).forEach((key) => {
    const conceptUuid = concepts[key];
    const value = measurements[key];
    // Only emit obs for fields that are in the catalog and have a value.
    if (conceptUuid && present(value)) {
      obs.push({ concept: conceptUuid, value });
    }
  });

  const openmrsEncounter = {
    patient: data.person_openmrs_uuid,
    encounterType: cfg.encounterType,
    location: cfg.location,
    encounterDatetime: (data.encounter_date || '').slice(0, 10) || null,
    encounterProviders: cfg.provider ? [{ provider: cfg.provider }] : [],
    obs,
  };

  console.log(
    'Transformed E-Heza prenatal encounter',
    data.encounter_uuid,
    '->',
    obs.length,
    'obs'
  );

  return { ...state, openmrsEncounter };
});
