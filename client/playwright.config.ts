import { defineConfig, devices } from '@playwright/test';
import { getClientPort } from './e2e/helpers/client-port';

const recording = !!process.env.RECORD;

export default defineConfig({
  globalSetup: './e2e/global-setup.ts',
  globalTeardown: recording ? './e2e/global-teardown.ts' : undefined,
  testDir: './e2e',
  // A committed test.only would otherwise shrink a shard to one test and
  // leave the job green, and e2e is the only functional gate on a release.
  forbidOnly: !!process.env.CI,
  timeout: 120000,
  retries: 0,
  workers: 1,
  use: {
    baseURL: `http://localhost:${getClientPort()}`,
    headless: !recording,
    ignoreHTTPSErrors: true,
    screenshot: 'only-on-failure',
    trace: 'on-first-retry',
    video: recording ? 'on' : 'off',
    ...devices['iPad Mini'],
    ...(recording ? { deviceScaleFactor: 1, viewport: { width: 820, height: 1024 } } : {}),
    // Pin the browser timezone to UTC so the app's date handling and any
    // date-sensitive assertions are deterministic across CI runners.
    timezoneId: 'UTC',
    hasTouch: false,
    isMobile: false,
  },
  projects: [
    { name: 'chromium', use: { browserName: 'chromium' } },
  ],
  webServer: undefined,
});
