#!/usr/bin/env node

/* One english string is written once. Every other id shows it with
   `translationSet ThatId`. Two copies drift: one gains a Kinyarwanda string
   and the other stays in english. */

'use strict';

const fs = require('fs');
const path = require('path');

const FILES = [
  'client/src/elm/Translate.elm',
  'server/elm/src/Translate.elm',
];

// English still written out twice. Folding one needs someone who reads the
// languages, so the list only shrinks: nothing may be added to it.
const TO_FOLD = path.join(__dirname, 'translations_to_fold.txt');

// English written twice on purpose, because the other languages say it
// differently in each place. The ids must fill in the same languages.
const SHARED = [
  {
    english: 'and',
    ids: ['And', 'AndSentence'],
    // Kinyarwanda joins two things with "na" and two clauses with "maze".
  },
  {
    english: 'You were diagnosed with',
    ids: ['DiagnosedAtAnotherFacilityPrefix', 'SpecialityCareHeaderPrefix'],
    // Each opens a sentence that ends differently, and Kinyarwanda names the
    // illness in one of them.
  },
  {
    english: 'Remember: Every day may not be good, but there will be something good in every day.',
    ids: ['ResilienceMessageConnecting1Paragraph3', 'ResilienceMessageGrowth5Paragraph2'],
    // Two resilience messages close on this line, each in its own Kinyarwanda.
  },
];

const LANGUAGES = ['kinyarwanda', 'kirundi', 'somali'];

const START = /^(\s*)\{ english = "((?:[^"\\]|\\.)*)"$/;
const CLOSE = /^\s*\}$/;
const HEAD = /^(\s*)([A-Z][A-Za-z0-9_.]*)[^\n]* ->$/;
const field = (language) => new RegExp(`^\\s*, ${language} = (Nothing|Just ".*")$`);

// Every written-out translation, at any depth: the id whose branch it sits in,
// and the languages it fills in. `translationSet SomeId` writes nothing out.
function records(source) {
  const lines = source.split('\n');
  const found = [];

  lines.forEach((line, i) => {
    // An empty english shows nothing, so it is skipped.
    const start = START.exec(line);
    if (!start || !start[2] || !CLOSE.test(lines[i + 4] || '')) {
      return;
    }

    const filled = [];
    for (let k = 0; k < LANGUAGES.length; k += 1) {
      const value = field(LANGUAGES[k]).exec(lines[i + 1 + k] || '');
      if (!value) {
        return;
      }
      if (value[1] !== 'Nothing') {
        filled.push(LANGUAGES[k]);
      }
    }

    const indent = start[1].length;
    let id = '?';
    for (let j = i - 1; j >= 0; j -= 1) {
      if (!lines[j].trim()) {
        continue;
      }
      const head = HEAD.exec(lines[j]);
      if (head && head[1].length === indent - 4) {
        id = head[2];
      }
      break;
    }

    found.push({ id, line: i + 1, english: start[2], filled: filled.join(', ') });
  });

  return found;
}

const shorten = (english) => (english.length > 60 ? `${english.slice(0, 60)}…` : english);

const toFold = new Set(fs.readFileSync(TO_FOLD, 'utf8').split('\n')
  .filter((line) => line.trim() && !line.startsWith('#')));

const used = new Set();
const offenders = [];
let written = 0;

for (const file of FILES) {
  const byEnglish = new Map();
  for (const record of records(fs.readFileSync(file, 'utf8'))) {
    written += 1;
    if (!byEnglish.has(record.english)) {
      byEnglish.set(record.english, []);
    }
    byEnglish.get(record.english).push(record);
  }

  for (const [english, ids] of byEnglish) {
    if (ids.length < 2) {
      continue;
    }

    const shared = SHARED.find((entry) => entry.english === english);
    if (shared) {
      used.add(shared.english);
      const listed = [...shared.ids].sort().join(', ');
      const found = ids.map((entry) => entry.id).sort().join(', ');
      if (listed !== found) {
        offenders.push({ file, english, ids, reason: `is shared by other ids than SHARED lists (${listed})` });
      } else if (new Set(ids.map((entry) => entry.filled)).size > 1) {
        offenders.push({ file, english, ids, reason: 'is shared, but the ids do not fill in the same languages' });
      }
      continue;
    }

    if (toFold.has(english)) {
      used.add(english);
      continue;
    }

    offenders.push({ file, english, ids, reason: 'is written out under more than one id' });
  }
}

for (const entry of SHARED) {
  if (!used.has(entry.english)) {
    offenders.push({ english: entry.english, ids: [], reason: 'is listed in SHARED but is no longer written out twice' });
  }
}

for (const english of toFold) {
  if (!used.has(english)) {
    offenders.push({ english, ids: [], reason: 'is listed in translations_to_fold.txt but is no longer written out twice — drop the line' });
  }
}

const left = [...toFold].filter((english) => used.has(english)).length;

if (offenders.length === 0) {
  console.log(`✓ ${written} translations written out, each english under one id`);
  console.log(`  ${left} english strings still to fold, listed in ${path.basename(TO_FOLD)}`);
  process.exit(0);
}

console.log(`✗ ${offenders.length} english string(s) to fix`);
for (const { file, english, ids, reason } of offenders) {
  console.log(`    "${shorten(english)}" ${reason}`);
  for (const { id, line, filled } of ids) {
    console.log(`        ${id} (${file}:${line}) — ${filled || 'english only'}`);
  }
}
console.log('    Write the english once, and read it elsewhere with `translationSet TheIdThatHoldsIt`.');
process.exit(1);
