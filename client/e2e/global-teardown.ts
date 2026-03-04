import { execSync } from 'child_process';
import { readdirSync, statSync } from 'fs';
import { join } from 'path';

/**
 * Playwright global teardown: converts recorded .webm videos to .mp4
 * using ffmpeg (when RECORD=1 is set).
 */
export default function globalTeardown() {
  if (!process.env.RECORD) return;

  const resultsDir = join(__dirname, '..', 'test-results');
  try {
    convertWebmFiles(resultsDir);
  } catch {
    // Don't fail the test run if conversion fails.
  }
}

function convertWebmFiles(dir: string) {
  for (const entry of readdirSync(dir)) {
    const full = join(dir, entry);
    if (statSync(full).isDirectory()) {
      convertWebmFiles(full);
    } else if (entry.endsWith('.webm')) {
      const mp4 = full.replace(/\.webm$/, '.mp4');
      try {
        execSync(`ffmpeg -i "${full}" -c:v libx264 -y "${mp4}"`, {
          stdio: 'ignore',
          timeout: 60000,
        });
        console.log(`Converted: ${full} → ${mp4}`);
      } catch {
        console.error(`Failed to convert: ${full}`);
      }
    }
  }
}
