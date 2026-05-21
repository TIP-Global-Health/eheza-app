/**
 * Load step — act on the matching decision and record the result back on
 * the E-Heza person. See integration/load-step.md.
 *
 * Input  (state.data): the E-Heza person payload (person_uuid).
 *        (state.openmrsPatient): the transform output — the POST body.
 *        (state.match): { action: 'link' | 'create', patientUuid }.
 *        (state.configuration): OpenMRS URL + auth, E-Heza write-back URL
 *        + shared secret.
 * Output (state.loadResult): { action, patientUuid, openmrsId }.
 *
 * Adaptor: @openfn/language-http
 */

// Standard Luhn check digit for a string of decimal digits.
const luhnCheckDigit = (digits) => {
  let sum = 0;
  let double = true; // the rightmost base digit is doubled
  for (let i = digits.length - 1; i >= 0; i--) {
    let d = digits.charCodeAt(i) - 48;
    if (double) {
      d *= 2;
      if (d > 9) d -= 9;
    }
    sum += d;
    double = !double;
  }
  return (10 - (sum % 10)) % 10;
};

/**
 * A deterministic, Luhn-valid OpenMRS ID for a person.
 *
 * The person UUID's hex is reduced modulo 10^12 (kept exact with plain
 * numbers — `n * 16 + 15` never exceeds Number.MAX_SAFE_INTEGER), zero
 * padded to 12 digits, then a hyphen and a standard-Luhn check digit are
 * appended — the format the OpenMRS LuhnIdentifierValidator accepts.
 */
const luhnOpenmrsId = (personUuid) => {
  const hex = String(personUuid).replace(/-/g, '');
  let n = 0;
  for (let i = 0; i < hex.length; i++) {
    n = (n * 16 + parseInt(hex[i], 16)) % 1000000000000;
  }
  const base = String(n).padStart(12, '0');
  return base + '-' + luhnCheckDigit(base);
};

/**
 * Return a copy of the patient body with every `autoGenerate` identifier
 * replaced by `openmrsId`; other identifiers (the national ID) pass through.
 */
const resolveIdentifiers = (patient, openmrsId) => ({
  ...patient,
  identifiers: (patient.identifiers || []).map((id) => {
    if (!id.autoGenerate) {
      return id;
    }
    const resolved = { ...id, identifier: openmrsId };
    delete resolved.autoGenerate;
    return resolved;
  }),
});

fn(async (state) => {
  const person = state.data || {};
  const match = state.match || {};
  const cfg = state.configuration || {};

  let patientUuid;
  let openmrsId = null;

  if (match.action === 'link') {
    patientUuid = match.patientUuid;
  } else if (match.action === 'create') {
    let patient = state.openmrsPatient || {};
    const needsId = (patient.identifiers || []).some((id) => id.autoGenerate);
    if (needsId) {
      openmrsId = luhnOpenmrsId(person.person_uuid);
      patient = resolveIdentifiers(patient, openmrsId);
    }
    const res = await post(cfg.openmrsBaseUrl + '/patient', {
      body: patient,
      headers: {
        Authorization: cfg.openmrsAuth,
        'Content-Type': 'application/json',
      },
    })(state);
    patientUuid = res.data && res.data.uuid;
    if (!patientUuid) {
      throw new Error('OpenMRS patient create returned no uuid');
    }
  } else {
    throw new Error('load: unexpected match action ' + match.action);
  }

  // Record the OpenMRS patient on the E-Heza person — for create and link.
  await post(cfg.ehezaPatientLinkUrl, {
    body: { person_uuid: person.person_uuid, openmrs_uuid: patientUuid },
    headers: {
      'X-OpenFN-Token': cfg.ehezaToken,
      'Content-Type': 'application/json',
    },
  })(state);

  console.log('Loaded', person.person_uuid, '->', match.action, patientUuid);

  return {
    ...state,
    data: person,
    loadResult: { action: match.action, patientUuid, openmrsId },
  };
});
