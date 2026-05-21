/**
 * Tests for jobs/match.js — the OpenMRS identity-matching step.
 *
 *   node --test integration/openfn/jobs/match.test.js
 *
 * The job does HTTP through the adaptor's `get` operation. The test loads
 * the job in the current realm with `fn` captured and `get` faked, so the
 * real tiering logic runs against canned OpenMRS search responses — only
 * the network boundary is stubbed.
 */

'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');

// Canned OpenMRS search responses, keyed by query; reset per test.
// 'id:<value>'  -> response for GET /patient?identifier=<value>
// 'q:<value>'   -> response for GET /patient?q=<value>
let searchResponses = {};

/** Stand-in for the HTTP adaptor's `get` — returns an operation. */
function fakeGet(_path, options) {
  return (state) => {
    const query = (options && options.query) || {};
    const key =
      query.identifier != null ? 'id:' + query.identifier : 'q:' + query.q;
    return { ...state, data: searchResponses[key] || { results: [] } };
  };
}

/**
 * Evaluate match.js in the current realm, returning its async operation.
 *
 * `get` stays on the global after load: the job references it lazily, when
 * the operation runs, not while the file is being evaluated.
 */
function loadMatch() {
  const src = fs.readFileSync(path.join(__dirname, 'match.js'), 'utf8');
  let operation;
  globalThis.fn = (f) => {
    operation = f;
  };
  globalThis.get = fakeGet;
  try {
    vm.runInThisContext(src, { filename: 'match.js' });
  } finally {
    delete globalThis.fn;
  }
  return operation;
}

const match = loadMatch();

/** Run the job, silencing its audit logging. */
const run = async (state) => {
  const log = console.log;
  console.log = () => {};
  try {
    return await match(state);
  } finally {
    console.log = log;
  }
};

const omrsPatient = (uuid, birthdate) => ({
  uuid,
  display: uuid + ' - patient',
  person: { uuid: 'person-' + uuid, gender: 'M', birthdate },
});

const PERSON_WITH_ID = {
  person_uuid: 'eh-1',
  first_name: 'Jean',
  second_name: 'Uwimana',
  national_id: '1234567890123456',
  birth_date: '1988-03-04 00:00:00',
  gender: 'male',
};

const PERSON_NO_ID = {
  person_uuid: 'eh-2',
  first_name: 'Aline',
  second_name: 'Mukamana',
  national_id: null,
  birth_date: '2019-07-15 00:00:00',
  gender: 'female',
};

test.beforeEach(() => {
  searchResponses = {};
});

test('national ID with exactly one hit → link via national-id', async () => {
  searchResponses['id:1234567890123456'] = {
    results: [omrsPatient('omrs-jean', '1988-03-04T00:00:00.000+0000')],
  };
  const { match: m } = await run({ data: PERSON_WITH_ID });
  assert.equal(m.action, 'link');
  assert.equal(m.via, 'national-id');
  assert.equal(m.patientUuid, 'omrs-jean');
});

test('national ID with more than one hit → ambiguous, throws', async () => {
  searchResponses['id:1234567890123456'] = {
    results: [
      omrsPatient('omrs-a', '1988-03-04T00:00:00.000+0000'),
      omrsPatient('omrs-b', '1988-03-04T00:00:00.000+0000'),
    ],
  };
  await assert.rejects(run({ data: PERSON_WITH_ID }), /ambiguous/i);
});

test('national ID misses, name + birth date hits once → link via name-birthdate', async () => {
  searchResponses['id:1234567890123456'] = { results: [] };
  searchResponses['q:Jean Uwimana'] = {
    results: [omrsPatient('omrs-jean', '1988-03-04T00:00:00.000+0000')],
  };
  const { match: m } = await run({ data: PERSON_WITH_ID });
  assert.equal(m.action, 'link');
  assert.equal(m.via, 'name-birthdate');
  assert.equal(m.patientUuid, 'omrs-jean');
});

test('no national ID, name + birth date hits once → link', async () => {
  searchResponses['q:Aline Mukamana'] = {
    results: [omrsPatient('omrs-aline', '2019-07-15T00:00:00.000+0000')],
  };
  const { match: m } = await run({ data: PERSON_NO_ID });
  assert.equal(m.action, 'link');
  assert.equal(m.via, 'name-birthdate');
  assert.equal(m.patientUuid, 'omrs-aline');
});

test('name hits but no birth date matches → create', async () => {
  searchResponses['q:Aline Mukamana'] = {
    results: [omrsPatient('omrs-other', '2001-01-01T00:00:00.000+0000')],
  };
  const { match: m } = await run({ data: PERSON_NO_ID });
  assert.equal(m.action, 'create');
  assert.equal(m.via, 'none');
  assert.equal(m.patientUuid, null);
});

test('more than one name + birth date match → ambiguous, throws', async () => {
  searchResponses['q:Aline Mukamana'] = {
    results: [
      omrsPatient('omrs-x', '2019-07-15T00:00:00.000+0000'),
      omrsPatient('omrs-y', '2019-07-15T00:00:00.000+0000'),
    ],
  };
  await assert.rejects(run({ data: PERSON_NO_ID }), /ambiguous/i);
});

test('no birth date but a name hit exists → ambiguous, throws', async () => {
  const noDob = { ...PERSON_NO_ID, birth_date: null };
  searchResponses['q:Aline Mukamana'] = {
    results: [omrsPatient('omrs-aline', '2019-07-15T00:00:00.000+0000')],
  };
  await assert.rejects(run({ data: noDob }), /ambiguous/i);
});

test('no birth date and no name hit → create', async () => {
  const noDob = { ...PERSON_NO_ID, birth_date: null };
  const { match: m } = await run({ data: noDob });
  assert.equal(m.action, 'create');
  assert.equal(m.via, 'none');
});

test('leaves state.data and state.openmrsPatient intact', async () => {
  searchResponses['id:1234567890123456'] = {
    results: [omrsPatient('omrs-jean', '1988-03-04T00:00:00.000+0000')],
  };
  const patient = { person: { names: [] }, identifiers: [] };
  const out = await run({ data: PERSON_WITH_ID, openmrsPatient: patient });
  assert.deepEqual(out.data, PERSON_WITH_ID);
  assert.deepEqual(out.openmrsPatient, patient);
});

test('records the candidates considered, for the audit trail', async () => {
  searchResponses['id:1234567890123456'] = {
    results: [omrsPatient('omrs-jean', '1988-03-04T00:00:00.000+0000')],
  };
  const { match: m } = await run({ data: PERSON_WITH_ID });
  assert.equal(m.candidates.length, 1);
  assert.equal(m.candidates[0].uuid, 'omrs-jean');
  assert.equal(m.candidates[0].birthdate, '1988-03-04');
});
