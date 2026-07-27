import { FullConfig } from '@playwright/test';
import { execFileSync } from 'child_process';
import { copyFileSync, existsSync, mkdirSync, readdirSync, rmSync, statSync, unlinkSync } from 'fs';
import { dirname, join, relative } from 'path';

// The directory beside the output directory where past runs are kept.
const KEPT_IN = 'e2e-recordings';

// How many runs to keep there. A video is several megabytes, and without a
// limit the directory grows until someone notices the disk.
const RUNS_KEPT = 10;

// Names this file gives the runs it keeps: the time the run ended. Anything
// else in the directory was put there by hand and is left alone.
const RUN_NAME = /^\d{4}-\d{2}-\d{2}T\d{2}-\d{2}-\d{2}-\d{3}Z$/;

/**
 * Playwright global teardown, run when RECORD=1 is set.
 *
 * Playwright empties its output directory when a run starts, so the video of a
 * recorded run was deleted by the run after it. Copy the videos somewhere no
 * run empties, and convert them to .mp4 there if ffmpeg is about.
 *
 * The videos are copied rather than moved. Reporters run after this and print
 * where they left each video, and the HTML report copies them out of the output
 * directory, so what is in there has to stay where it was written.
 */
export default function globalTeardown(config: FullConfig) {
  if (!process.env.RECORD) return;

  const stamp = new Date().toISOString().replace(/[:.]/g, '-');

  new Set(config.projects.map((project) => project.outputDir)).forEach((dir) => {
    try {
      const videos = existsSync(dir) ? findVideos(dir) : [];
      if (videos.length === 0) {
        console.log(`No videos to keep from ${dir}`);
        return;
      }
      const keptIn = join(dirname(dir), KEPT_IN);
      const runDir = join(keptIn, stamp);
      const kept = videos.filter((video) => keep(video, join(runDir, relative(dir, video))));
      if (kept.length === 0) {
        // An empty directory would take one of the places kept for a run.
        rmSync(runDir, { recursive: true, force: true });
        return;
      }
      console.log(`Recordings kept in ${runDir}`);
      dropOldestRuns(keptIn);
    } catch (error) {
      console.error(`Could not keep the recordings from ${dir}: ${error}`);
    }
  });
}

/**
 * Copies one video, and converts the copy to .mp4 when ffmpeg can.
 * Says whether the video was kept, so one that could not be does not stop
 * the rest.
 */
function keep(video: string, destination: string): boolean {
  try {
    mkdirSync(dirname(destination), { recursive: true });
    copyFileSync(video, destination);
  } catch (error) {
    console.error(`Could not keep ${video}: ${error}`);
    return false;
  }

  const mp4 = destination.replace(/\.webm$/, '.mp4');
  try {
    execFileSync('ffmpeg', ['-i', destination, '-c:v', 'libx264', '-y', mp4], {
      stdio: 'ignore',
      timeout: 60000,
    });
  } catch {
    // ffmpeg makes the file it is asked for before it reads anything, so a
    // conversion that stops part way leaves one that will not play.
    rmSync(mp4, { force: true });
    console.log(`Kept: ${destination} (no .mp4: ffmpeg is missing, or could not convert it)`);
    return true;
  }

  // Keeping both formats of the same video would double what is on disk.
  // The video is kept either way, so a delete that fails is said and passed
  // over rather than taking the videos after it with it.
  try {
    unlinkSync(destination);
  } catch (error) {
    console.error(`Could not remove ${destination}: ${error}`);
  }
  console.log(`Kept: ${mp4}`);
  return true;
}

function dropOldestRuns(keptIn: string) {
  const runs = readdirSync(keptIn)
    .filter((entry) => RUN_NAME.test(entry) && statSync(join(keptIn, entry)).isDirectory())
    .sort();
  runs.slice(0, Math.max(0, runs.length - RUNS_KEPT)).forEach((run) => {
    rmSync(join(keptIn, run), { recursive: true, force: true });
    console.log(`Removed older recording: ${join(keptIn, run)}`);
  });
}

function findVideos(dir: string): string[] {
  return readdirSync(dir).flatMap((entry) => {
    const full = join(dir, entry);
    if (statSync(full).isDirectory()) return findVideos(full);
    return entry.endsWith('.webm') ? [full] : [];
  });
}
