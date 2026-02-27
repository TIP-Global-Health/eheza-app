import { defineConfig, devices } from '@playwright/test';

const recording = !!process.env.RECORD;

export default defineConfig({
  globalSetup: './e2e/global-setup.ts',
  testDir: './e2e',
  timeout: 120000,
  retries: 1,
  use: {
    baseURL: 'http://localhost:3000',
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
