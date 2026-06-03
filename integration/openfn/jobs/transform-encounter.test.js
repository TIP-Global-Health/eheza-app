/**
 * Tests for jobs/transform-encounter.js.
 *
 *   node --test integration/openfn/jobs/transform-encounter.test.js
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

const CONFIG = {
  encounterType: 'enc-type-uuid',
  location: 'loc-uuid',
  provider: 'prov-uuid',
  prenatalConcepts: {
    hemoglobin_count: 'concept-hgb',
    execution_date: 'concept-exec-date',
    blood_smear_result: 'concept-smear',
  },
};

test('builds an encounter with one obs per present measurement', () => {
  const job = loadJob('transform-encounter.js');
  const state = {
    configuration: CONFIG,
    data: {
      encounter_uuid: 'enc-uuid',
      encounter_date: '2026-05-01T00:00:00',
      person_openmrs_uuid: 'patient-uuid',
      measurements: {
        hemoglobin_count: 11.2,
        blood_smear_result: 'positive',
        not_in_catalog: 'ignored',
      },
    },
  };
  const out = job(state);
  const enc = out.openmrsEncounter;
  assert.equal(enc.patient, 'patient-uuid');
  assert.equal(enc.encounterType, 'enc-type-uuid');
  assert.equal(enc.location, 'loc-uuid');
  assert.equal(enc.encounterDatetime, '2026-05-01');
  assert.deepEqual(enc.encounterProviders, [{ provider: 'prov-uuid' }]);

  const byConcept = Object.fromEntries(enc.obs.map((o) => [o.concept, o.value]));
  assert.equal(byConcept['concept-hgb'], 11.2);
  assert.equal(byConcept['concept-smear'], 'positive');
  // Field absent from the catalog is dropped.
  assert.equal(enc.obs.length, 2);
});

test('omits measurements with empty values', () => {
  const job = loadJob('transform-encounter.js');
  const out = job({
    configuration: CONFIG,
    data: {
      encounter_uuid: 'e', encounter_date: '2026-05-01',
      person_openmrs_uuid: 'p',
      measurements: { hemoglobin_count: '', blood_smear_result: 'negative' },
    },
  });
  assert.equal(out.openmrsEncounter.obs.length, 1);
  assert.equal(out.openmrsEncounter.obs[0].concept, 'concept-smear');
});

test('accepts prenatalConcepts as a JSON string (Lightning credential form)', () => {
  const job = loadJob('transform-encounter.js');
  const out = job({
    configuration: {
      ...CONFIG,
      prenatalConcepts: JSON.stringify(CONFIG.prenatalConcepts),
    },
    data: {
      encounter_uuid: 'e', encounter_date: '2026-05-01',
      person_openmrs_uuid: 'p',
      measurements: { hemoglobin_count: 11.2, blood_smear_result: 'positive' },
    },
  });
  const byConcept = Object.fromEntries(
    out.openmrsEncounter.obs.map((o) => [o.concept, o.value])
  );
  assert.equal(byConcept['concept-hgb'], 11.2);
  assert.equal(byConcept['concept-smear'], 'positive');
  assert.equal(out.openmrsEncounter.obs.length, 2);
});
