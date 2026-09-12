#!/usr/bin/env node

/* ---------------------------------------------------------------------------- *
 *
 * Report translation ids that share an english string but not their translations.
 *
 * The same english text under two ids is fine, and often deliberate: a helper
 * sentence repeated per diagnosis is one id per diagnosis so each can be worded
 * differently later. What is not fine is one of them carrying Kinyarwanda,
 * Kirundi or Somali while another carries none: whoever added the second copy
 * did not see the first, and the screen it feeds falls back to english.
 *
 * ---------------------------------------------------------------------------- */

'use strict';

const fs = require('fs');

const FILES = [
  'client/src/elm/Translate.elm',
  'server/elm/src/Translate.elm',
];

// A translation branch: the constructor, its english, and whether each of the
// other three languages is filled in.
const BRANCH = new RegExp(
  '^ {8}([A-Za-z_][^\\n]*?) ->\\n' +
  '\\s*\\{ english = "((?:[^"\\\\]|\\\\.)*)"\\n' +
  '\\s*, kinyarwanda = (Nothing|Just [^\\n]*)\\n' +
  '\\s*, kirundi = (Nothing|Just [^\\n]*)\\n' +
  '\\s*, somali = (Nothing|Just [^\\n]*)\\n',
  'gm'
);

let failures = 0;

for (const file of FILES) {
  if (!fs.existsSync(file)) {
    console.error(`✗ ${file} is missing`);
    failures += 1;
    continue;
  }

  const source = fs.readFileSync(file, 'utf8');
  const byEnglish = new Map();

  for (const match of source.matchAll(BRANCH)) {
    const constructor = match[1].trim().split(/\s+/)[0];
    const english = match[2];
    const translated = [match[3], match[4], match[5]].filter((value) => value !== 'Nothing').length;

    if (!byEnglish.has(english)) {
      byEnglish.set(english, []);
    }
    byEnglish.get(english).push({ constructor, translated });
  }

  const offenders = [];
  for (const [english, ids] of byEnglish) {
    if (ids.length < 2) {
      continue;
    }
    const translations = ids.map((id) => id.translated);
    if (Math.max(...translations) > 0 && Math.min(...translations) === 0) {
      offenders.push({ english, ids });
    }
  }

  if (offenders.length === 0) {
    console.log(`✓ ${file}: ${byEnglish.size} english strings, none translated under one id and not another`);
    continue;
  }

  failures += offenders.length;
  console.log(`✗ ${file}: ${offenders.length} english string(s) translated under one id and not another`);
  for (const { english, ids } of offenders) {
    const shown = english.length > 60 ? `${english.slice(0, 60)}…` : english;
    console.log(`    "${shown}"`);
    for (const id of ids) {
      console.log(`        ${id.constructor} — ${id.translated}/3 translated`);
    }
    console.log('      Use the id that carries the translations, or translate the other.');
  }
}

process.exit(failures === 0 ? 0 : 1);
