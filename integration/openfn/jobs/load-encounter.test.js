/**
 * Tests for jobs/load-encounter.js — the OpenMRS encounter load step.
 *
 *   node --test integration/openfn/jobs/load-encounter.test.js
 *
 * The job POSTs through the adaptor's `post` operation and, on replace,
 * voids the previous encounter through the adaptor's `del` operation. The
 * test loads the job in the current realm with `fn` captured and `post`/`del`
 * faked, so the real branching and write-back run — only the network is
 * stubbed. Faked calls are recorded for inspection. Mirrors load.test.js's
 * stubbing.
 */

'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');

let postCalls = [];
let delCalls = [];

function reset() {
  postCalls = [];
  delCalls = [];
}

/** Stand-in for the HTTP adaptor's `post(path, data, options)` — records the call. */
function fakePost(url, data, options) {
  return (state) => {
    postCalls.push({
      url,
      body: data || null,
      headers: (options && options.headers) || {},
    });
    // The link URL contains `/encounter-link`; check it FIRST. Otherwise a
    // URL containing `/encounter` is the create call, which returns a uuid.
    const str = String(url);
    if (str.includes('/encounter-link')) {
      return { ...state, data: { status: 'ok' } };
    }
    if (str.includes('/encounter')) {
      return { ...state, data: { uuid: 'new-enc-uuid' } };
    }
    return { ...state, data: {} };
  };
}

/** Stand-in for the HTTP adaptor's `del(path, options)` — records the call. */
function fakeDel(url, options) {
  return (state) => {
    delCalls.push({
      url,
      headers: (options && options.headers) || {},
    });
    return { ...state, data: {} };
  };
}

/** Evaluate load-encounter.js in the current realm, returning its async operation. */
function loadJob(file) {
  const src = fs.readFileSync(path.join(__dirname, file), 'utf8');
  let operation;
  globalThis.fn = (f) => {
    operation = f;
  };
  globalThis.post = fakePost;
  globalThis.del = fakeDel;
  try {
    vm.runInThisContext(src, { filename: file });
  } finally {
    delete globalThis.fn;
  }
  const log = console.log;
  const job = async (state) => {
    console.log = () => {};
    try {
      return await operation(state);
    } finally {
      console.log = log;
    }
  };
  return { job };
}

test('create branch: posts encounter then links back; no delete', async () => {
  reset();
  const { job } = loadJob('load-encounter.js');
  const out = await job({
    configuration: {
      openmrsBaseUrl: 'http://openmrs/ws/rest/v1',
      openmrsAuth: 'auth',
      ehezaEncounterLinkUrl: 'http://eheza/openmrs/encounter-link',
      ehezaToken: 'secret',
    },
    data: { encounter_uuid: 'eheza-enc-uuid' },
    openmrsEncounter: { patient: 'p', obs: [] },
    encounterMatch: { action: 'create', patientUuid: 'p' },
  });

  assert.equal(delCalls.length, 0);
  assert.equal(postCalls.length, 2);
  assert.ok(postCalls[0].url.indexOf('/encounter') !== -1);
  assert.ok(postCalls[1].url.indexOf('/encounter-link') !== -1);
  assert.equal(postCalls[1].body.openmrs_uuid, 'new-enc-uuid');
  assert.equal(postCalls[1].body.encounter_uuid, 'eheza-enc-uuid');
  assert.equal(out.loadResult.action, 'created');
});

test('replace branch: deletes previous encounter, then creates + links back', async () => {
  reset();
  const { job } = loadJob('load-encounter.js');
  const out = await job({
    configuration: {
      openmrsBaseUrl: 'http://openmrs/ws/rest/v1',
      openmrsAuth: 'auth',
      ehezaEncounterLinkUrl: 'http://eheza/openmrs/encounter-link',
      ehezaToken: 'secret',
    },
    data: { encounter_uuid: 'eheza-enc-uuid' },
    openmrsEncounter: { patient: 'p', obs: [] },
    encounterMatch: { action: 'replace', patientUuid: 'p', previousEncounterUuid: 'old-uuid' },
  });

  assert.equal(delCalls.length, 1);
  assert.ok(delCalls[0].url.indexOf('/encounter/old-uuid') !== -1);
  assert.equal(postCalls.length, 2);
  assert.ok(postCalls[0].url.indexOf('/encounter') !== -1);
  assert.equal(postCalls[1].body.openmrs_uuid, 'new-enc-uuid');
  assert.equal(out.loadResult.action, 'replaced');
});
