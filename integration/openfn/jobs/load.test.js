/**
 * Tests for jobs/load.js — the OpenMRS load step.
 *
 *   node --test integration/openfn/jobs/load.test.js
 *
 * The job POSTs through the adaptor's `post` operation. The test loads the
 * job in the current realm with `fn` captured and `post` faked, so the real
 * branching, identifier resolution and write-back run — only the network is
 * stubbed. Faked POSTs are recorded for inspection.
 */

'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');

let postCalls = [];
let getResponse = null;
let getCalls = [];

/** Stand-in for the HTTP adaptor's `post(path, data, options)` — records the call. */
function fakePost(url, data, options) {
  return (state) => {
    postCalls.push({
      url,
      body: data || null,
      headers: (options && options.headers) || {},
    });
    const response = String(url).endsWith('/patient')
      ? { uuid: 'omrs-created-uuid' }
      : {};
    return { ...state, data: response };
  };
}

/** Stand-in for the HTTP adaptor's `get(path, options)` — used by the update branch. */
function fakeGet(url, options) {
  return (state) => {
    getCalls.push({ url, headers: (options && options.headers) || {} });
    return { ...state, data: getResponse };
  };
}

/** Evaluate load.js in the current realm, returning its async operation. */
function loadJob() {
  const src = fs.readFileSync(path.join(__dirname, 'load.js'), 'utf8');
  let operation;
  globalThis.fn = (f) => {
    operation = f;
  };
  globalThis.post = fakePost;
  globalThis.get = fakeGet;
  try {
    vm.runInThisContext(src, { filename: 'load.js' });
  } finally {
    delete globalThis.fn;
  }
  return operation;
}

const load = loadJob();

const run = async (state) => {
  const log = console.log;
  console.log = () => {};
  try {
    return await load(state);
  } finally {
    console.log = log;
  }
};

/** Independent standard-Luhn check, to verify generated IDs. */
function luhnValid(idWithCheck) {
  const parts = String(idWithCheck).split('-');
  if (parts.length !== 2) return false;
  const [base, check] = parts;
  let sum = 0;
  let double = true;
  for (let i = base.length - 1; i >= 0; i--) {
    let d = base.charCodeAt(i) - 48;
    if (double) {
      d *= 2;
      if (d > 9) d -= 9;
    }
    sum += d;
    double = !double;
  }
  return (10 - (sum % 10)) % 10 === Number(check);
}

const CONFIG = {
  openmrsBaseUrl: 'http://omrs.test/ws/rest/v1',
  openmrsAuth: 'Basic dGVzdA==',
  ehezaPatientLinkUrl: 'http://eheza.test/openmrs/patient-link',
  ehezaToken: 'shared-secret',
};

const PERSON = { person_uuid: 'ccc36870-b7d7-5244-85ac-4ed1c26cad08' };

const patientWithNationalId = () => ({
  person: { names: [{ givenName: 'Ariella' }], gender: 'F' },
  identifiers: [
    {
      identifier: '1199270166285559',
      identifierType: 'natid-type',
      location: 'loc',
      preferred: true,
    },
  ],
});

const patientWithAutoGenerate = () => ({
  person: { names: [{ givenName: 'Aline' }], gender: 'F' },
  identifiers: [
    {
      identifierType: 'omrsid-type',
      location: 'loc',
      autoGenerate: true,
      preferred: true,
    },
  ],
});

const createCall = () => postCalls.find((c) => c.url.endsWith('/patient'));
const writeBack = () =>
  postCalls.find((c) => c.url === CONFIG.ehezaPatientLinkUrl);

test.beforeEach(() => {
  postCalls = [];
  getCalls = [];
  getResponse = null;
});

/** A canned "current patient state" returned by the fake GET on update. */
const existingPatient = (overrides = {}) => ({
  uuid: 'omrs-patient-uuid',
  identifiers: overrides.identifiers || [
    { uuid: 'id-uuid', identifierType: { uuid: 'natid-type' } },
  ],
  person: {
    uuid: 'omrs-person-uuid',
    addresses: overrides.addresses || [{ uuid: 'addr-uuid' }],
    attributes: overrides.attributes || [
      { uuid: 'attr-uuid', attributeType: { uuid: 'civil_status_type' } },
    ],
    preferredName: overrides.preferredName === undefined
      ? { uuid: 'name-uuid' }
      : overrides.preferredName,
  },
});

const desiredPatient = () => ({
  person: {
    names: [{ givenName: 'Aline', familyName: 'Mukamana', preferred: true }],
    gender: 'F',
    birthdate: '2019-07-15',
    birthdateEstimated: false,
    attributes: [{ attributeType: 'civil_status_type', value: 'Married' }],
  },
  identifiers: [
    { identifier: '1234567890123456', identifierType: 'natid-type', location: 'loc', preferred: true },
  ],
});

const updateState = (over = {}) => ({
  data: { person_uuid: 'eh-uuid', ...over.data },
  openmrsPatient: over.openmrsPatient || desiredPatient(),
  match: { action: 'update', patientUuid: 'omrs-patient-uuid', ...(over.match || {}) },
  configuration: CONFIG,
});

const find = (predicate) => postCalls.find(predicate);

test('link → no OpenMRS create, just the write-back', async () => {
  await run({
    data: PERSON,
    openmrsPatient: patientWithNationalId(),
    match: { action: 'link', patientUuid: 'omrs-existing' },
    configuration: CONFIG,
  });
  assert.equal(createCall(), undefined);
  assert.equal(postCalls.length, 1);
});

test('link → writes the matched UUID back to E-Heza', async () => {
  const out = await run({
    data: PERSON,
    openmrsPatient: patientWithNationalId(),
    match: { action: 'link', patientUuid: 'omrs-existing' },
    configuration: CONFIG,
  });
  assert.equal(writeBack().body.person_uuid, PERSON.person_uuid);
  assert.equal(writeBack().body.openmrs_uuid, 'omrs-existing');
  assert.equal(out.loadResult.action, 'link');
  assert.equal(out.loadResult.patientUuid, 'omrs-existing');
});

test('create → POSTs the patient then writes the new UUID back', async () => {
  const out = await run({
    data: PERSON,
    openmrsPatient: patientWithNationalId(),
    match: { action: 'create', patientUuid: null },
    configuration: CONFIG,
  });
  assert.ok(createCall(), 'expected a POST /patient');
  assert.equal(writeBack().body.openmrs_uuid, 'omrs-created-uuid');
  assert.equal(out.loadResult.patientUuid, 'omrs-created-uuid');
});

test('create → a national-ID identifier is sent unchanged', async () => {
  await run({
    data: PERSON,
    openmrsPatient: patientWithNationalId(),
    match: { action: 'create' },
    configuration: CONFIG,
  });
  const ids = createCall().body.identifiers;
  assert.equal(ids.length, 1);
  assert.equal(ids[0].identifier, '1199270166285559');
});

test('create → an autoGenerate placeholder becomes a resolved OpenMRS ID', async () => {
  await run({
    data: PERSON,
    openmrsPatient: patientWithAutoGenerate(),
    match: { action: 'create' },
    configuration: CONFIG,
  });
  const id = createCall().body.identifiers[0];
  assert.match(id.identifier, /^\d{12}-\d$/);
  assert.equal(id.autoGenerate, undefined, 'the flag must be dropped');
  assert.equal(id.identifierType, 'omrsid-type');
});

test('the generated OpenMRS ID has a valid Luhn check digit', async () => {
  const out = await run({
    data: PERSON,
    openmrsPatient: patientWithAutoGenerate(),
    match: { action: 'create' },
    configuration: CONFIG,
  });
  assert.ok(luhnValid(out.loadResult.openmrsId), out.loadResult.openmrsId);
});

test('OpenMRS ID generation is deterministic for the same person', async () => {
  const opts = {
    data: PERSON,
    openmrsPatient: patientWithAutoGenerate(),
    match: { action: 'create' },
    configuration: CONFIG,
  };
  const a = await run(opts);
  postCalls = [];
  const b = await run(opts);
  assert.equal(a.loadResult.openmrsId, b.loadResult.openmrsId);
});

test('distinct persons get distinct OpenMRS IDs', async () => {
  const a = await run({
    data: { person_uuid: 'ccc36870-b7d7-5244-85ac-4ed1c26cad08' },
    openmrsPatient: patientWithAutoGenerate(),
    match: { action: 'create' },
    configuration: CONFIG,
  });
  postCalls = [];
  const b = await run({
    data: { person_uuid: '11330210-dafc-551e-b163-99ce5b0d3f4c' },
    openmrsPatient: patientWithAutoGenerate(),
    match: { action: 'create' },
    configuration: CONFIG,
  });
  assert.notEqual(a.loadResult.openmrsId, b.loadResult.openmrsId);
});

test('the write-back carries the shared-secret header', async () => {
  await run({
    data: PERSON,
    openmrsPatient: patientWithNationalId(),
    match: { action: 'link', patientUuid: 'omrs-existing' },
    configuration: CONFIG,
  });
  assert.equal(writeBack().headers['X-OpenFN-Token'], 'shared-secret');
});

test('does not mutate state.openmrsPatient', async () => {
  const patient = patientWithAutoGenerate();
  await run({
    data: PERSON,
    openmrsPatient: patient,
    match: { action: 'create' },
    configuration: CONFIG,
  });
  assert.equal(patient.identifiers[0].autoGenerate, true);
  assert.equal(patient.identifiers[0].identifier, undefined);
});

// -- Update branch --

test('update GETs the patient once to discover subresource UUIDs', async () => {
  getResponse = existingPatient();
  await run(updateState());
  assert.equal(getCalls.length, 1);
  assert.ok(getCalls[0].url.includes('/patient/omrs-patient-uuid'));
});

test('update posts the person-level patch (gender, birthdate, estimated)', async () => {
  getResponse = existingPatient();
  await run(updateState());
  const personPatch = find((c) => c.url === CONFIG.openmrsBaseUrl + '/person/omrs-person-uuid');
  assert.ok(personPatch, 'expected POST /person/{personUuid}');
  assert.deepEqual(personPatch.body, {
    gender: 'F',
    birthdate: '2019-07-15',
    birthdateEstimated: false,
  });
});

test('update skips the gender "U" sentinel — preserves OpenMRS gender', async () => {
  getResponse = existingPatient();
  const desired = desiredPatient();
  desired.person.gender = 'U';
  await run(updateState({ openmrsPatient: desired }));
  const personPatch = find((c) => c.url === CONFIG.openmrsBaseUrl + '/person/omrs-person-uuid');
  assert.equal(personPatch.body.gender, undefined);
});

test('update skips a null birthdate — preserves OpenMRS birthdate', async () => {
  getResponse = existingPatient();
  const desired = desiredPatient();
  desired.person.birthdate = null;
  await run(updateState({ openmrsPatient: desired }));
  const personPatch = postCalls.find((c) => c.url === CONFIG.openmrsBaseUrl + '/person/omrs-person-uuid');
  // With only gender to send, the patch posts but without birthdate.
  if (personPatch) {
    assert.equal(personPatch.body.birthdate, undefined);
    assert.equal(personPatch.body.birthdateEstimated, undefined);
  }
});

test('update posts the name to the existing preferredName subresource', async () => {
  getResponse = existingPatient();
  await run(updateState());
  const nameCall = find((c) => c.url === CONFIG.openmrsBaseUrl + '/person/omrs-person-uuid/name/name-uuid');
  assert.ok(nameCall, 'expected POST /person/{uuid}/name/{nameUuid}');
  assert.equal(nameCall.body.givenName, 'Aline');
  assert.equal(nameCall.body.familyName, 'Mukamana');
});

test('update creates a new name when no preferredName exists', async () => {
  getResponse = existingPatient({ preferredName: null });
  await run(updateState());
  const nameCreate = find((c) => c.url === CONFIG.openmrsBaseUrl + '/person/omrs-person-uuid/name');
  assert.ok(nameCreate, 'expected POST /person/{uuid}/name (create)');
  assert.equal(nameCreate.body.preferred, true);
});

test('update posts the address to the existing address subresource', async () => {
  getResponse = existingPatient();
  const desired = desiredPatient();
  desired.person.addresses = [{ stateProvince: 'Amajyaruguru', country: 'Rwanda' }];
  await run(updateState({ openmrsPatient: desired }));
  const addrCall = find((c) => c.url === CONFIG.openmrsBaseUrl + '/person/omrs-person-uuid/address/addr-uuid');
  assert.ok(addrCall, 'expected POST /person/{uuid}/address/{addressUuid}');
  assert.equal(addrCall.body.country, 'Rwanda');
});

test('update creates a new address when none exists in OpenMRS', async () => {
  getResponse = existingPatient({ addresses: [] });
  const desired = desiredPatient();
  desired.person.addresses = [{ stateProvince: 'Kigali' }];
  await run(updateState({ openmrsPatient: desired }));
  const addrCreate = find((c) => c.url === CONFIG.openmrsBaseUrl + '/person/omrs-person-uuid/address');
  assert.ok(addrCreate, 'expected POST /person/{uuid}/address (create)');
});

test('update upserts an identifier by its type — to existing /identifier/{uuid}', async () => {
  getResponse = existingPatient();
  await run(updateState());
  const idCall = find((c) => c.url === CONFIG.openmrsBaseUrl + '/patient/omrs-patient-uuid/identifier/id-uuid');
  assert.ok(idCall);
  assert.equal(idCall.body.identifier, '1234567890123456');
});

test('update posts a new identifier when its type is not in OpenMRS', async () => {
  getResponse = existingPatient({ identifiers: [] });
  await run(updateState());
  const idCreate = find((c) => c.url === CONFIG.openmrsBaseUrl + '/patient/omrs-patient-uuid/identifier');
  assert.ok(idCreate);
  assert.equal(idCreate.body.identifier, '1234567890123456');
});

test('update skips autoGenerate identifier placeholders', async () => {
  getResponse = existingPatient({ identifiers: [] });
  const desired = desiredPatient();
  desired.identifiers = [
    { identifierType: 'omrsid-type', location: 'loc', autoGenerate: true, preferred: true },
  ];
  await run(updateState({ openmrsPatient: desired }));
  const idCall = postCalls.find((c) => c.url.includes('/identifier'));
  assert.equal(idCall, undefined);
});

test('update upserts an attribute by its type — to existing /attribute/{uuid}', async () => {
  getResponse = existingPatient();
  await run(updateState());
  const attrCall = find((c) => c.url === CONFIG.openmrsBaseUrl + '/person/omrs-person-uuid/attribute/attr-uuid');
  assert.ok(attrCall);
  assert.equal(attrCall.body.value, 'Married');
});

test('update posts a new attribute when its type is not in OpenMRS', async () => {
  getResponse = existingPatient({ attributes: [] });
  await run(updateState());
  const attrCreate = find((c) => c.url === CONFIG.openmrsBaseUrl + '/person/omrs-person-uuid/attribute');
  assert.ok(attrCreate);
  assert.equal(attrCreate.body.value, 'Married');
});

test('update does not call the E-Heza write-back endpoint', async () => {
  getResponse = existingPatient();
  const out = await run(updateState());
  assert.equal(writeBack(), undefined);
  assert.equal(out.loadResult.action, 'update');
  assert.equal(out.loadResult.patientUuid, 'omrs-patient-uuid');
});
