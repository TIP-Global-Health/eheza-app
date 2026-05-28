/**
 * Load step — act on the matching decision and record the result back on
 * the E-Heza person. See integration/load-step.md.
 *
 * Input  (state.data): the E-Heza person payload (person_uuid).
 *        (state.openmrsPatient): the transform output — the POST body.
 *        (state.match): { action: 'link' | 'create' | 'update', patientUuid }.
 *        (state.configuration): OpenMRS URL + auth, E-Heza write-back URL
 *        + shared secret.
 * Output (state.loadResult): { action, patientUuid, openmrsId }.
 *        On 'update' the write-back endpoint is not called — the OpenMRS
 *        UUID is unchanged. See integration/patient-update.md.
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
 *
 * Collisions between two distinct person UUIDs are mathematically
 * possible (modulo reduction), but extremely unlikely at the PoC's
 * population scale. For UVL production this branch should be replaced
 * by an idgen-driven identifier (see integration/load-step.md).
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

/**
 * Apply the transform's desired patient body onto an existing OpenMRS
 * patient — additive, never destructive (Phase 1.5 policy):
 *   - Empty source fields → no write; OpenMRS's value is preserved.
 *   - Non-empty source fields → POST to the matching OpenMRS subresource.
 *   - identifiers / attributes upserted by their `identifierType` /
 *     `attributeType` (one per type, the OpenMRS convention).
 *   - `autoGenerate` placeholders are skipped (only meaningful on create).
 *
 * One GET discovers the subresource UUIDs; everything after is POSTs.
 */
const updatePatient = async (state, cfg, patientUuid) => {
  const desired = state.openmrsPatient || {};
  const desiredPerson = desired.person || {};
  const authHeaders = { Authorization: cfg.openmrsAuth };

  const rep =
    'custom:(uuid,identifiers:(uuid,identifierType:(uuid)),person:' +
    '(uuid,addresses:(uuid),attributes:(uuid,attributeType:(uuid)),preferredName:(uuid)))';
  // Pass `v` through the adaptor's `query` option so reserved chars in
  // the custom-rep value are encoded — same pattern as match.js.
  const res = await get(
    cfg.openmrsBaseUrl + '/patient/' + patientUuid,
    { query: { v: rep }, headers: authHeaders }
  )(state);
  const got = res.data || {};
  const gotPerson = got.person || {};
  const personUuid = gotPerson.uuid;
  if (!personUuid) {
    throw new Error('update: GET /patient returned no person uuid');
  }

  const personPath = cfg.openmrsBaseUrl + '/person/' + personUuid;
  const patientPath = cfg.openmrsBaseUrl + '/patient/' + patientUuid;

  // Person-level patch. Gender 'U' is the transform's fallback for an
  // empty source — skip it so OpenMRS keeps its real value.
  const personPatch = {};
  if (desiredPerson.gender && desiredPerson.gender !== 'U') {
    personPatch.gender = desiredPerson.gender;
  }
  if (desiredPerson.birthdate) {
    personPatch.birthdate = desiredPerson.birthdate;
    personPatch.birthdateEstimated = !!desiredPerson.birthdateEstimated;
  }
  if (Object.keys(personPatch).length > 0) {
    await post(personPath, personPatch, { headers: authHeaders })(state);
  }

  // Name — upsert the preferred name.
  const desiredName = (desiredPerson.names || [])[0];
  if (desiredName) {
    const namePatch = {};
    if (desiredName.givenName) namePatch.givenName = desiredName.givenName;
    if (desiredName.familyName) namePatch.familyName = desiredName.familyName;
    if (Object.keys(namePatch).length > 0) {
      const nameUuid = gotPerson.preferredName && gotPerson.preferredName.uuid;
      if (nameUuid) {
        await post(personPath + '/name/' + nameUuid, namePatch, {
          headers: authHeaders,
        })(state);
      } else {
        await post(
          personPath + '/name',
          { ...namePatch, preferred: true },
          { headers: authHeaders }
        )(state);
      }
    }
  }

  // Address — one PersonAddress per person here; upsert it.
  const desiredAddress = (desiredPerson.addresses || [])[0];
  if (desiredAddress) {
    const addrUuid = ((gotPerson.addresses || [])[0] || {}).uuid;
    if (addrUuid) {
      await post(personPath + '/address/' + addrUuid, desiredAddress, {
        headers: authHeaders,
      })(state);
    } else {
      await post(personPath + '/address', desiredAddress, {
        headers: authHeaders,
      })(state);
    }
  }

  // Identifiers — upsert by identifierType; skip autoGenerate placeholders.
  for (const id of desired.identifiers || []) {
    if (id.autoGenerate) {
      continue;
    }
    const existing = (got.identifiers || []).find(
      (i) => i.identifierType && i.identifierType.uuid === id.identifierType
    );
    if (existing) {
      await post(
        patientPath + '/identifier/' + existing.uuid,
        { identifier: id.identifier },
        { headers: authHeaders }
      )(state);
    } else {
      await post(patientPath + '/identifier', id, {
        headers: authHeaders,
      })(state);
    }
  }

  // PersonAttributes — upsert by attributeType.
  for (const attr of desiredPerson.attributes || []) {
    const existing = (gotPerson.attributes || []).find(
      (a) => a.attributeType && a.attributeType.uuid === attr.attributeType
    );
    if (existing) {
      await post(
        personPath + '/attribute/' + existing.uuid,
        { value: attr.value },
        { headers: authHeaders }
      )(state);
    } else {
      await post(personPath + '/attribute', attr, {
        headers: authHeaders,
      })(state);
    }
  }
};

fn(async (state) => {
  const person = state.data || {};
  const match = state.match || {};
  const cfg = state.configuration || {};

  let patientUuid;
  let openmrsId = null;

  if (match.action === 'update') {
    patientUuid = match.patientUuid;
    await updatePatient(state, cfg, patientUuid);
    console.log('Loaded', person.person_uuid, '->', match.action, patientUuid);
    return {
      ...state,
      data: person,
      loadResult: { action: 'update', patientUuid, openmrsId: null },
    };
  } else if (match.action === 'link') {
    patientUuid = match.patientUuid;
  } else if (match.action === 'create') {
    let patient = state.openmrsPatient || {};
    const needsId = (patient.identifiers || []).some((id) => id.autoGenerate);
    if (needsId) {
      openmrsId = luhnOpenmrsId(person.person_uuid);
      patient = resolveIdentifiers(patient, openmrsId);
    }
    const res = await post(cfg.openmrsBaseUrl + '/patient', patient, {
      headers: { Authorization: cfg.openmrsAuth },
    })(state);
    patientUuid = res.data && res.data.uuid;
    if (!patientUuid) {
      throw new Error('OpenMRS patient create returned no uuid');
    }
  } else {
    throw new Error('load: unexpected match action ' + match.action);
  }

  // Record the OpenMRS patient on the E-Heza person — for create and link.
  await post(
    cfg.ehezaPatientLinkUrl,
    { person_uuid: person.person_uuid, openmrs_uuid: patientUuid },
    { headers: { 'X-OpenFN-Token': cfg.ehezaToken } }
  )(state);

  console.log('Loaded', person.person_uuid, '->', match.action, patientUuid);

  return {
    ...state,
    data: person,
    loadResult: { action: match.action, patientUuid, openmrsId },
  };
});
