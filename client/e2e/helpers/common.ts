import { Page } from '@playwright/test';
import { click } from './auth';

/**
 * Click the sync icon, wait for sync to complete on a health center,
 * then navigate back.
 *
 * After the initial "Status: Success" is detected, waits 1 second and
 * re-verifies the status is still "Success" to guard against false
 * positives (e.g., download completes but upload is still in progress).
 *
 * @param page - Playwright page
 * @param healthCenter - Health center name to wait for (default: 'Nyange Health Center')
 * @param timeout - Max time in ms to wait for sync success (default: 300000)
 */
export async function syncAndWait(
  page: Page,
  healthCenter = 'Nyange Health Center',
  timeout = 300000,
) {
  await click(page.locator('span.sync-icon'), page);

  await page.locator('.device-status').waitFor({ timeout: 10000 });

  const hcSection = page.locator('.health-center', {
    has: page.locator('h2', { hasText: healthCenter }),
  });
  await hcSection.waitFor({ timeout: 10000 });

  // Wait for sync to complete.
  await hcSection
    .locator('.sync-status', { hasText: 'Status: Success' })
    .waitFor({ timeout });

  // Verify status is stable (not a transient state between phases).
  await page.waitForTimeout(1000);
  await hcSection
    .locator('.sync-status', { hasText: 'Status: Success' })
    .waitFor({ timeout: 5000 });

  await page.goBack();
}
