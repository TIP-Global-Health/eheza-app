/**
 * Tests for jobs/transform.js — the E-Heza person -> OpenMRS patient transform.
 *
 * The transform is an OpenFN job file: a script that calls `fn(operation)` in
 * a runtime-injected scope, with no exports. To test it in isolation we
 * evaluate the file in a sandbox that captures the operation, then run it.
 *
 *   node --test integration/openfn/jobs/
 *
 * Sample payloads use the real key names and real Drupal `allowed_values`
 * (verified against the live person node + field config).
 */

'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');

/**
 * Evaluate transform.js and return its `(state) => state` operation.
 *
 * The job file has no exports — it calls `fn(operation)` in a runtime-injected
 * scope. We run it in the current realm (so `deepStrictEqual` accepts the
 * objects it builds) with a temporary global `fn` that captures the operation.
 */
function loadTransform() {
  const src = fs.readFileSync(path.join(__dirname, 'transform.js'), 'utf8');
  let operation;
  globalThis.fn = (f) => {
    operation = f;
  };
  try {
    vm.runInThisContext(src, { filename: 'transform.js' });
  } finally {
    delete globalThis.fn;
  }
  return operation;
}

const transform = loadTransform();

/** Run the transform, returning the patient body; the job's logging is silenced. */
const run = (person) => {
  const log = console.log;
  console.log = () => {};
  try {
    return transform({ data: person }).openmrsPatient;
  } finally {
    console.log = log;
  }
};

// A real payload, straight from hedley_openmrs_build_person_payload (nid 6).
const REAL_WITH_ID = {
  person_uuid: '11330210-dafc-551e-b163-99ce5b0d3f4c',
  first_name: 'Ariella',
  second_name: 'Iradukunda',
  national_id: '1199270166285559',
  birth_date: '1995-12-31 00:00:00',
  gender: 'female',
  hiv_status: null,
  marital_status: null,
  mode_of_delivery: null,
  education_level: null,
  phone_number: '0715566915',
  province: 'Amajyaruguru',
  district: 'Gakenke',
  sector: 'Coko',
  cell: 'Mbirima',
  village: 'Akanduga',
  latitude: null,
  longitude: null,
  spouse_name: null,
  spouse_phone_number: null,
  next_of_kin_name: null,
  next_of_kin_phone_number: null,
  number_of_children: null,
  birth_date_estimated: false,
};

// Every optional field populated, with real Drupal allowed_values.
const FULL = {
  person_uuid: 'test-uuid-full',
  first_name: 'Jean',
  second_name: 'Uwimana',
  national_id: '1234567890123456',
  birth_date: '1988-03-04 00:00:00',
  gender: 'male',
  hiv_status: 'negative-dc',
  marital_status: 'living-with-partner',
  mode_of_delivery: 'svd-episiotomy',
  education_level: '3',
  phone_number: '0788123456',
  province: 'Kigali',
  district: 'Gasabo',
  sector: 'Remera',
  cell: 'Rukiri',
  village: 'Amahoro',
  latitude: '-1.95',
  longitude: '30.10',
  spouse_name: 'Marie',
  spouse_phone_number: '0788000111',
  next_of_kin_name: 'Paul',
  next_of_kin_phone_number: '0788222333',
  number_of_children: '2',
  birth_date_estimated: true,
};

// A realistic person with no national ID (e.g. a child) — the identifier gap.
const MINIMAL_NO_ID = {
  person_uuid: 'test-uuid-noid',
  first_name: 'Aline',
  second_name: 'Mukamana',
  national_id: null,
  birth_date: '2019-07-15 00:00:00',
  gender: 'female',
  hiv_status: null,
  marital_status: null,
  mode_of_delivery: null,
  education_level: null,
  phone_number: null,
  province: 'Amajyaruguru',
  district: 'Gakenke',
  sector: 'Coko',
  cell: 'Mbirima',
  village: 'Akanduga',
  latitude: null,
  longitude: null,
  spouse_name: null,
  spouse_phone_number: null,
  next_of_kin_name: null,
  next_of_kin_phone_number: null,
  number_of_children: null,
  birth_date_estimated: false,
};

test('maps name to a single preferred PersonName', () => {
  const p = run(REAL_WITH_ID);
  assert.deepEqual(p.person.names, [
    { givenName: 'Ariella', familyName: 'Iradukunda', preferred: true },
  ]);
});

test('maps gender via the value map', () => {
  assert.equal(run(REAL_WITH_ID).person.gender, 'F');
  assert.equal(run(FULL).person.gender, 'M');
});

test('truncates birthdate to an ISO date and carries the estimated flag', () => {
  assert.equal(run(REAL_WITH_ID).person.birthdate, '1995-12-31');
  assert.equal(run(REAL_WITH_ID).person.birthdateEstimated, false);
  assert.equal(run(FULL).person.birthdate, '1988-03-04');
  assert.equal(run(FULL).person.birthdateEstimated, true);
});

test('maps the Rwanda address hierarchy onto PersonAddress slots', () => {
  assert.deepEqual(run(REAL_WITH_ID).person.addresses, [
    {
      stateProvince: 'Amajyaruguru',
      countyDistrict: 'Gakenke',
      address3: 'Coko',
      address2: 'Mbirima',
      cityVillage: 'Akanduga',
    },
  ]);
});

test('value maps resolve every clinical list field', () => {
  const attrs = run(FULL).person.attributes;
  const byValue = attrs.map((a) => a.value);
  assert.ok(byValue.includes('Negative (discordant couple)'), 'hivStatus');
  assert.ok(byValue.includes('Living with partner'), 'maritalStatus');
  assert.ok(
    byValue.includes('Spontaneous vaginal, with episiotomy'),
    'modeOfDelivery'
  );
  assert.ok(byValue.includes('Secondary school'), 'educationLevel');
});

test('omits PersonAttributes for empty source fields', () => {
  // REAL_WITH_ID has only a phone number among the attribute-backed fields.
  const attrs = run(REAL_WITH_ID).person.attributes;
  assert.equal(attrs.length, 1);
  assert.equal(attrs[0].value, '0715566915');
});

test('emits a national-ID identifier when national_id is present', () => {
  const ids = run(REAL_WITH_ID).identifiers;
  assert.equal(ids.length, 1);
  assert.equal(ids[0].identifier, '1199270166285559');
  assert.equal(ids[0].preferred, true);
});

test('every patient payload carries at least one identifier', () => {
  // OpenMRS rejects a patient with no identifier. A person with no national
  // ID (a child, an unregistered adult) must still produce a valid patient.
  const ids = run(MINIMAL_NO_ID).identifiers;
  assert.ok(ids.length >= 1, 'expected >=1 identifier, got ' + ids.length);
});

test('falls back to an auto-generated OpenMRS ID when there is no national ID', () => {
  // The transform cannot call idgen (no HTTP) — it marks intent with an
  // autoGenerate placeholder; the load step resolves the value via idgen.
  const ids = run(MINIMAL_NO_ID).identifiers;
  assert.equal(ids.length, 1);
  assert.equal(ids[0].autoGenerate, true);
  assert.equal(ids[0].identifierType, '8d793bee-c2cc-11de-8d13-0010c6dffd0f');
  assert.equal(ids[0].preferred, true);
  assert.equal(ids[0].identifier, undefined, 'value is resolved by the load step');
});

test('does not add the OpenMRS-ID fallback when a national ID is present', () => {
  const ids = run(REAL_WITH_ID).identifiers;
  assert.equal(ids.length, 1);
  assert.equal(ids[0].autoGenerate, undefined);
});
