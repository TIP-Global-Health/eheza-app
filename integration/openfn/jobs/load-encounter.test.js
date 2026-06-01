/**
 * Tests for jobs/load-encounter.js — the OpenMRS encounter load step.
 *
 *   node --test integration/openfn/jobs/load-encounter.test.js
 *
 * The job POSTs through the adaptor's `post` operation. The test loads the
 * job in the current realm with `fn` captured and `post` faked, so the real
 * branching and write-back run — only the network is stubbed. Faked POSTs
 * are recorded for inspection. Mirrors load.test.js's stubbing.
 */

'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');

let postCalls = [];

/** Stand-in for the HTTP adaptor's `post(path, data, options)` — records the call. */
function fakePost(url, data, options) {
  return (state) => {
    postCalls.push({
      url,
      body: data || null,
      headers: (options && options.headers) || {},
    });
    // The link URL contains `/encounter-link`; check it FIRST. Otherwise a
    // URL ending in `/encounter` is the create call, which returns a uuid.
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

/** Evaluate load-encounter.js in the current realm, returning its async operation. */
function loadJob(file) {
  const src = fs.readFileSync(path.join(__dirname, file), 'utf8');
  let operation;
  globalThis.fn = (f) => {
    operation = f;
  };
  globalThis.post = fakePost;
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

test.beforeEach(() => {
  postCalls = [];
});

test('create branch: posts encounter then links back', async () => {
  const { job } = loadJob('load-encounter.js');
  const out = await job({
    configuration: {
      openmrsBaseUrl: 'http://openmrs/ws/rest/v1',
      ehezaEncounterLinkUrl: 'http://eheza/openmrs/encounter-link',
      ehezaToken: 'secret',
    },
    data: { encounter_uuid: 'eheza-enc-uuid' },
    openmrsEncounter: { patient: 'p', obs: [] },
    encounterMatch: { action: 'create', patientUuid: 'p' },
  });

  assert.equal(postCalls.length, 2, 'expected two POSTs');

  const create = postCalls[0];
  const link = postCalls[1];

  assert.ok(create.url.endsWith('/encounter'), 'first POST is the encounter create');
  assert.ok(link.url.includes('/encounter-link'), 'second POST is the link-back');

  assert.equal(link.body.openmrs_uuid, 'new-enc-uuid');
  assert.equal(link.body.encounter_uuid, 'eheza-enc-uuid');

  assert.equal(out.loadResult.action, 'created');
  assert.equal(out.loadResult.openmrsUuid, 'new-enc-uuid');
});

test('skip branch: posts nothing', async () => {
  const { job } = loadJob('load-encounter.js');
  const out = await job({
    configuration: {
      openmrsBaseUrl: 'http://openmrs/ws/rest/v1',
      ehezaEncounterLinkUrl: 'http://eheza/openmrs/encounter-link',
      ehezaToken: 'secret',
    },
    data: { encounter_uuid: 'eheza-enc-uuid' },
    openmrsEncounter: { patient: 'p', obs: [] },
    encounterMatch: { action: 'skip', encounterUuid: 'already' },
  });

  assert.equal(postCalls.length, 0, 'expected no POSTs on skip');
  assert.equal(out.loadResult.action, 'skipped');
  assert.equal(out.loadResult.openmrsUuid, 'already');
});
