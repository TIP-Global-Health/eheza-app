import { defineConfig, devices } from '@playwright/test';
import { getClientPort } from './e2e/helpers/client-port';

const recording = !!process.env.RECORD;

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
