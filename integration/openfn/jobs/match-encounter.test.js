/**
 * Tests for jobs/match-encounter.js.
 *
 *   node --test integration/openfn/jobs/match-encounter.test.js
 */
'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');

function loadJob(file) {
  const code = fs.readFileSync(path.join(__dirname, file), 'utf8');
  let captured;
  // Evaluate in the current realm (as the other job tests do) so the objects
  // the job builds share this realm's prototypes and `deepEqual` accepts them.
  const log = console.log;
  globalThis.fn = (f) => {
    captured = f;
  };
  console.log = () => {};
  try {
    vm.runInThisContext(code, { filename: file });
  } finally {
    delete globalThis.fn;
    console.log = log;
  }
  return captured;
}

test('marks create when no existing encounter UUID', () => {
  const job = loadJob('match-encounter.js');
  const out = job({ data: { person_openmrs_uuid: 'p', existing_encounter_uuid: '' } });
  assert.equal(out.encounterMatch.action, 'create');
  assert.equal(out.encounterMatch.patientUuid, 'p');
});

test('short-circuits to skip when already linked', () => {
  const job = loadJob('match-encounter.js');
  const out = job({
    data: { person_openmrs_uuid: 'p', existing_encounter_uuid: 'already-there' },
  });
  assert.equal(out.encounterMatch.action, 'skip');
  assert.equal(out.encounterMatch.encounterUuid, 'already-there');
});

test('throws when the person is not linked', () => {
  const job = loadJob('match-encounter.js');
  assert.throws(
    () => job({ data: { person_openmrs_uuid: '', existing_encounter_uuid: '' } }),
    /person not linked/i
  );
});
