import { expect, Page } from '@playwright/test';
import { click } from './auth';

/**
 * Navigate to Case Management from the main menu (PinCodePage when logged in).
 * Works for all roles (nurse, CHW, lab tech).
 */
export async function navigateToCaseManagement(page: Page) {
  await click(page.locator('.icon-task-case-management'), page);
  await page.locator('.page-case-management').waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);
}

/**
 * Verify that a Case Management pane is visible with the expected heading,
 * and that a follow-up entry for the given patient name exists within it.
 *
 * @param filterText - Button text to click (e.g., "Home Visit", "Acute Illness").
 *                     Case-insensitive match since CSS uppercases the text.
 * @param paneHeadingText - Expected pane heading text (e.g., "Child Nutrition Follow Up").
 * @param patientName - The patient's display name to look for in follow-up entries.
 */
export async function verifyCaseManagementEntry(
  page: Page,
  filterText: string,
  paneHeadingText: string,
  patientName: string,
) {
  // Click the filter button. Escape filterText to avoid regex metacharacter issues.
  const escaped = filterText.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  const filterBtn = page.locator('div.ui.segment.filters button', {
    hasText: new RegExp(escaped, 'i'),
  });
  await click(filterBtn, page);
  await page.waitForTimeout(500);

  // Verify pane heading is visible.
  const paneHeading = page.locator('div.pane-heading', {
    hasText: paneHeadingText,
  });
  await expect(paneHeading).toBeVisible({ timeout: 5000 });

  // Scope entry search to the pane containing the heading to avoid
  // false positives when multiple panes are visible.
  const pane = page.locator('div.pane', { has: paneHeading });
  const entry = pane.locator('div.follow-up-entry', {
    hasText: patientName,
  });
  await expect(entry).toBeVisible({ timeout: 5000 });
}

/**
 * Verify that clicking the forward icon on a follow-up entry opens
 * the follow-up encounter dialog with the patient's name, then dismiss it.
 *
 * @param patientName - The patient's display name to find in the entry.
 */
export async function verifyFollowUpDialog(
  page: Page,
  patientName: string,
) {
  // Find the first matching entry and click its forward icon.
  const entry = page.locator('div.follow-up-entry', {
    hasText: patientName,
  }).first();
  await expect(entry).toBeVisible({ timeout: 5000 });
  await click(entry.locator('.icon-forward'), page);

  // Verify dialog appears with patient name.
  const modal = page.locator('div.ui.active.modal');
  await expect(modal).toBeVisible({ timeout: 5000 });
  await expect(modal.locator('.person-name')).toHaveText(patientName);

  // Dismiss by clicking "No".
  await click(modal.locator('button', { hasText: 'No' }), page);
  await modal.waitFor({ state: 'hidden', timeout: 5000 });
}
