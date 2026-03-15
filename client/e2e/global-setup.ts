import { resetDevice } from './helpers/device';
import { getClientPort } from './helpers/client-port';

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
      const controller = new AbortController();
      const timeoutId = setTimeout(() => controller.abort(), 4000);
      try {
        const res = await fetch(url, { signal: controller.signal });
        if (res.ok) {
          console.log(`Frontend ready at ${url}`);
          return;
        }
      } finally {
        clearTimeout(timeoutId);
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
