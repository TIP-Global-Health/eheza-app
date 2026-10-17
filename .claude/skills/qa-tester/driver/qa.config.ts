import { defineConfig } from '@playwright/test';
import { resolve } from 'path';

// Runs driver.ts as one endless "test": the runner is only used to load the
// TypeScript e2e helpers. Started by scripts/qa.sh, never by CI.
export default defineConfig({
  testDir: __dirname,
  testMatch: /driver\.ts$/,
  outputDir: resolve(__dirname, '../../../../client/qa-recordings/.driver/test-results'),
  timeout: 0,
  workers: 1,
  retries: 0,
  reporter: 'line',
});
