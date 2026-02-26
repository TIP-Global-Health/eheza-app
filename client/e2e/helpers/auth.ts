import { Page } from '@playwright/test';

/**
 * Wait for the app to finish initializing. The app goes through
 * several stages: service worker installation -> device pairing ->
 * PIN login. This function polls until either the pairing form or
 * the PIN form is visible (meaning the SW is active and Elm has
 * rendered).
 *
 * Returns 'pairing' or 'pin' depending on which page appeared.
 */
async function waitForAppReady(page: Page, timeout = 60000): Promise<'pairing' | 'pin'> {
  const deadline = Date.now() + timeout;
  while (Date.now() < deadline) {
    const pinVisible = await page.locator('input[name="pincode"]').isVisible().catch(() => false);
    if (pinVisible) return 'pin';

    const pairingVisible = await page.locator('input[name="pairing-code"]').isVisible().catch(() => false);
    if (pairingVisible) return 'pairing';

    await page.waitForTimeout(500);
  }
  throw new Error('App did not reach pairing or PIN page within timeout');
}

/**
 * Pair the device with the backend using a pairing code.
 * This is required on first launch (fresh browser context) before
 * the PIN login page becomes available.
 */
export async function pairDevice(page: Page, pairingCode = '99999999') {
  // Clear localStorage to remove any cached device pairing state,
  // which forces the app to show the pairing form. We keep the
  // service worker and its caches intact so nurse/health center
  // data remains available for PIN validation.
  await page.goto('/');
  await page.evaluate(() => {
    localStorage.clear();
    sessionStorage.clear();
  });
  await page.reload();

  const state = await waitForAppReady(page);

  // If PIN page still shows after clearing localStorage, something
  // unexpected is happening — skip pairing anyway.
  if (state === 'pin') {
    return;
  }

  // Fill the pairing code.
  const pairingInput = page.locator('input[name="pairing-code"]');
  await pairingInput.fill(pairingCode);

  // Wait for Elm to process the input and enable the submit button.
  const enabledSubmit = page.locator('button.ui.fluid.primary.button:not([disabled])');
  await enabledSubmit.waitFor({ timeout: 5000 });
  await enabledSubmit.click();

  // After successful pairing, the app auto-navigates to the PIN page.
  await page.locator('input[name="pincode"]').waitFor({ timeout: 60000 });
}

/**
 * Full login flow: pair device (if needed), enter nurse PIN,
 * and select a health center.
 *
 * After a fresh device pairing, the sync manager needs time to
 * download nurse data from the backend. This function retries the
 * PIN login until the data is available.
 */
export async function login(page: Page, pin = '1234') {
  await pairDevice(page);

  const pinInput = page.locator('input[name="pincode"]');
  const signInButton = page.getByRole('button', { name: 'Sign In' });

  // Retry PIN login — after fresh pairing, the sync manager needs
  // time to download nurse data before PIN validation works.
  for (let attempt = 0; attempt < 20; attempt++) {
    await pinInput.fill(pin);
    await signInButton.waitFor({ timeout: 5000 });
    await signInButton.click();

    // Wait briefly and check if we got past the PIN page.
    await page.waitForTimeout(3000);

    const selectLocation = await page.locator('p.select-location').isVisible().catch(() => false);
    if (selectLocation) break;

    const logoutButton = await page.locator('button.ui.button.logout').isVisible().catch(() => false);
    if (logoutButton) break;

    // Still on PIN page — sync likely hasn't completed yet.
    // Wait before retrying.
    if (attempt === 19) {
      throw new Error('PIN login failed after 20 retries — nurse data may not have synced');
    }
  }

  // If we see the health center selection, select the first one.
  const selectLocation = await page.locator('p.select-location').isVisible().catch(() => false);
  if (selectLocation) {
    await page.locator('p.select-location ~ button.ui.primary.button').first().click();
  }

  // Wait for the main dashboard to appear.
  await page.waitForSelector('button.ui.button.logout', { timeout: 30000 });
}
