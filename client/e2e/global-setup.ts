import { readFileSync } from 'fs';
import { resolve } from 'path';
import { resetDevice } from './helpers/device';

/**
 * Read the host port from .ddev/docker-compose.client-http.yaml.
 * Falls back to 3000 if the file is missing or unparseable.
 */
function getClientPort(): number {
  try {
    const yamlPath = resolve(__dirname, '../../.ddev/docker-compose.client-http.yaml');
    const content = readFileSync(yamlPath, 'utf-8');
    const match = content.match(/- (\d+):3000/);
    if (match) return parseInt(match[1], 10);
  } catch {
    // File not found or unreadable — use default.
  }
  return 3000;
}

/**
 * Wait for the frontend to be reachable on the configured port.
 * Retries every 5 seconds for up to 2 minutes.
 */
async function waitForFrontend() {
  const port = getClientPort();
  const url = `http://localhost:${port}/`;
  const maxAttempts = 24;
  const interval = 5000;

  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    try {
      const res = await fetch(url);
      if (res.ok) {
        console.log(`Frontend ready at ${url}`);
        return;
      }
    } catch {
      // Not ready yet.
    }
    if (attempt < maxAttempts) {
      console.log(`Waiting for frontend at ${url} (attempt ${attempt}/${maxAttempts})...`);
      await new Promise(r => setTimeout(r, interval));
    }
  }
  throw new Error(`Frontend not reachable at ${url} after ${maxAttempts * interval / 1000}s. Is ddev gulp running?`);
}

/**
 * Playwright global setup: verify the frontend is reachable, then
 * create a fresh device node with a known pairing code.
 */
export default async function globalSetup() {
  await waitForFrontend();
  resetDevice();
}
