import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';

test.describe('Smoke test', () => {
  test('setup device and sync Nyange Health Center', async ({ page }) => {
    await setupDevice(page);

    // Verify we're back on the dashboard after device setup.
    await expect(page.locator('button.ui.button.logout')).toBeVisible();
    await expect(page.locator('.wrap-cards')).toBeVisible();
  });
});
