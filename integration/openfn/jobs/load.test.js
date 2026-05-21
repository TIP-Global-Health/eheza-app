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

/** Stand-in for the HTTP adaptor's `post` — records the call, returns an op. */
function fakePost(url, options) {
  return (state) => {
    postCalls.push({
      url,
      body: (options && options.body) || null,
      headers: (options && options.headers) || {},
    });
    const data = String(url).endsWith('/patient')
      ? { uuid: 'omrs-created-uuid' }
      : {};
    return { ...state, data };
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
});

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
