import { test, expect } from '@playwright/test';
import { chmodSync, existsSync, mkdirSync, mkdtempSync, readdirSync, rmSync, writeFileSync } from 'fs';
import { tmpdir } from 'os';
import { join } from 'path';
import globalTeardown from './global-teardown';

/**
 * What the teardown does with the videos of a recorded run.
 *
 * It is called here directly, against directories made for each case, rather
 * than by recording a real test: the cases below are about a video that cannot
 * be copied or an ffmpeg that gives up part way, which a passing run does not
 * produce. No browser is started.
 */

const ROOT = mkdtempSync(join(tmpdir(), 'recordings-kept-'));
const REAL_PATH = process.env.PATH || '';

test.afterAll(() => rmSync(ROOT, { recursive: true, force: true }));

/** An ffmpeg that behaves in the given way, on a PATH of its own. */
function ffmpegThat(name: string, script: string) {
  const dir = join(ROOT, 'ffmpeg', name);
  mkdirSync(dir, { recursive: true });
  writeFileSync(join(dir, 'ffmpeg'), `#!/bin/bash\n${script}\n`, { mode: 0o755 });
  return dir;
}

const NAME_OF_OUTPUT = 'out=""\nwhile [ $# -gt 0 ]; do [ "$1" = "-y" ] && out="$2"; shift; done';
const CONVERTS = ffmpegThat('converts', `${NAME_OF_OUTPUT}\necho mp4 > "$out"`);
// ffmpeg makes the file it is asked for before it reads anything.
const GIVES_UP = ffmpegThat('gives-up', `${NAME_OF_OUTPUT}\necho half > "$out"\nexit 1`);
const IS_MISSING = ffmpegThat('is-missing', 'exit 127');

/** A run that recorded a video for each of the named tests. */
function aRunThatRecorded(name: string, tests: string[]) {
  const outputDir = join(ROOT, name, 'test-results');
  tests.forEach((one) => {
    mkdirSync(join(outputDir, one), { recursive: true });
    writeFileSync(join(outputDir, one, 'video.webm'), 'pretend video');
  });
  mkdirSync(outputDir, { recursive: true });
  return { outputDir, keptIn: join(ROOT, name, 'e2e-recordings') };
}

function tidyUp(outputDir: string, ffmpeg: string) {
  process.env.RECORD = '1';
  process.env.PATH = `${ffmpeg}:${REAL_PATH}`;
  try {
    globalTeardown({ projects: [{ outputDir }] } as never);
  } finally {
    process.env.PATH = REAL_PATH;
    delete process.env.RECORD;
  }
}

function runsKeptIn(keptIn: string) {
  return existsSync(keptIn) ? readdirSync(keptIn).sort() : [];
}

function filesIn(dir: string): string[] {
  return readdirSync(dir, { withFileTypes: true })
    .flatMap((entry) => (entry.isDirectory() ? filesIn(join(dir, entry.name)) : [entry.name]))
    .sort();
}

test('without ffmpeg the videos are kept as they are', () => {
  const { outputDir, keptIn } = aRunThatRecorded('no-ffmpeg', ['one', 'two']);
  tidyUp(outputDir, IS_MISSING);

  const [run] = runsKeptIn(keptIn);
  expect(filesIn(join(keptIn, run))).toEqual(['video.webm', 'video.webm']);
  // Reporters run after the teardown and print where they left each video.
  expect(existsSync(join(outputDir, 'one', 'video.webm'))).toBe(true);
});

test('with ffmpeg only the .mp4 is kept', () => {
  const { outputDir, keptIn } = aRunThatRecorded('with-ffmpeg', ['one']);
  tidyUp(outputDir, CONVERTS);

  const [run] = runsKeptIn(keptIn);
  expect(filesIn(join(keptIn, run))).toEqual(['video.mp4']);
});

test('a conversion that stops part way leaves no .mp4 behind', () => {
  const { outputDir, keptIn } = aRunThatRecorded('ffmpeg-gives-up', ['one']);
  tidyUp(outputDir, GIVES_UP);

  const [run] = runsKeptIn(keptIn);
  expect(filesIn(join(keptIn, run))).toEqual(['video.webm']);
});

test('a video that cannot be copied stops neither the others nor the pruning', () => {
  const { outputDir, keptIn } = aRunThatRecorded('one-unreadable', ['one', 'two']);
  chmodSync(join(outputDir, 'one', 'video.webm'), 0o000);
  for (let day = 1; day <= 12; day += 1) {
    mkdirSync(join(keptIn, `2020-01-${String(day).padStart(2, '0')}T00-00-00-000Z`), { recursive: true });
  }
  tidyUp(outputDir, IS_MISSING);
  chmodSync(join(outputDir, 'one', 'video.webm'), 0o644);

  const runs = runsKeptIn(keptIn);
  expect(runs).toHaveLength(10);
  expect(filesIn(join(keptIn, runs[runs.length - 1]))).toEqual(['video.webm']);
});

test('a recording moved there by hand is left alone', () => {
  const { outputDir, keptIn } = aRunThatRecorded('kept-by-hand', ['one']);
  mkdirSync(join(keptIn, 'issue-1982'), { recursive: true });
  writeFileSync(join(keptIn, 'issue-1982', 'video.webm'), 'kept by hand');
  for (let day = 1; day <= 12; day += 1) {
    mkdirSync(join(keptIn, `2020-01-${String(day).padStart(2, '0')}T00-00-00-000Z`), { recursive: true });
  }
  tidyUp(outputDir, IS_MISSING);

  expect(runsKeptIn(keptIn)).toContain('issue-1982');
});

test('when nothing can be kept no run is left behind', () => {
  const { outputDir, keptIn } = aRunThatRecorded('all-unreadable', ['one']);
  chmodSync(join(outputDir, 'one', 'video.webm'), 0o000);
  tidyUp(outputDir, IS_MISSING);
  chmodSync(join(outputDir, 'one', 'video.webm'), 0o644);

  expect(runsKeptIn(keptIn)).toEqual([]);
});

test('a run that recorded nothing keeps nothing', () => {
  const { outputDir, keptIn } = aRunThatRecorded('no-videos', []);
  tidyUp(outputDir, IS_MISSING);

  expect(runsKeptIn(keptIn)).toEqual([]);
});
