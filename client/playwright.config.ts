import { defineConfig, devices } from '@playwright/test';
import { readFileSync } from 'fs';
import { resolve } from 'path';

const recording = !!process.env.RECORD;

/**
 * Read the host port from .ddev/docker-compose.client-http.yaml.
 * Falls back to 3000 if the file is missing or unparseable.
 */
function getClientPort(): number {
  try {
    const yamlPath = resolve(__dirname, '../.ddev/docker-compose.client-http.yaml');
    const content = readFileSync(yamlPath, 'utf-8');
    const match = content.match(/- (\d+):3000/);
    if (match) return parseInt(match[1], 10);
  } catch {
    // File not found or unreadable — use default.
  }
  return 3000;
}

export default defineConfig({
  globalSetup: './e2e/global-setup.ts',
  globalTeardown: recording ? './e2e/global-teardown.ts' : undefined,
  testDir: './e2e',
  timeout: 120000,
  retries: 1,
  workers: 1,
  use: {
    baseURL: `http://localhost:${getClientPort()}`,
    headless: !recording,
    ignoreHTTPSErrors: true,
    screenshot: 'only-on-failure',
    trace: 'on-first-retry',
    video: recording ? 'on' : 'off',
    ...devices['iPad Mini'],
    hasTouch: false,
    isMobile: false,
  },
  projects: [
    { name: 'chromium', use: { browserName: 'chromium' } },
  ],
  webServer: undefined,
});
