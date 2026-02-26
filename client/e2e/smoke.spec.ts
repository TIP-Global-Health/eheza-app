import { test, expect } from '@playwright/test';
import { login } from './helpers/auth';

test.describe('Smoke test', () => {
  test('login and see dashboard', async ({ page }) => {
    await login(page);

    // Verify we see the main dashboard.
    await expect(page.locator('button.ui.button.logout')).toBeVisible();
    await expect(page.locator('.wrap-cards')).toBeVisible();
  });
});
