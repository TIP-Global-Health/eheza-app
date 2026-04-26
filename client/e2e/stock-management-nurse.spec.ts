import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import { syncAndWait } from './helpers/common';
import {
  navigateToStockManagement,
  completeReceiveStock,
  completeCorrectEntry,
  queryStockUpdateNodes,
  ensureStockManagementHCFeatureEnabled,
} from './helpers/stock-management';
import { click } from './helpers/auth';

// =========================================================================
// Test: Nurse Stock Management — Receive Stock, Correct Entries, View Details
// =========================================================================

// Scenario: Nurse performs all stock management operations at Nyange Health Center.
// Steps:
//   1. Navigate to Stock Management from main menu
//   2. Receive Stock: supplier=MOH, batch=BATCH-E2E-001, qty=100, with signature
//   3. Correct Entry (Subtraction): reason=Error in input, qty=5, with signature
//   4. Correct Entry (Addition): reason=Error in input, qty=3, with signature
//   5. View Month Details: verify table renders
//   6. Sync to backend
// Backend: Verifies 3 new stock_update nodes (1 receive-supply, 2 correction).

test.describe('Nurse: Stock Management — Full Flow', () => {
  test.describe.configure({ timeout: 300000 });

  test.beforeAll(() => {
    ensureStockManagementHCFeatureEnabled();
  });

  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page, '1234', 'Nyange Health Center');
  });

  test('receive stock, correct entry (subtraction + addition), view details, verify backend', async ({ page }) => {
    // Get baseline count of stock_update nodes before test.
    const baseline = queryStockUpdateNodes('Nyange Health Center');
    const initialCount = baseline.count;
    const initialReceiveSupply = baseline.types['receive-supply'] ?? 0;
    const initialCorrection = baseline.types['correction'] ?? 0;

    // 1. Navigate to Stock Management from the main menu.
    await navigateToStockManagement(page);

    // Verify ModeMain dashboard is displayed.
    await expect(page.locator('.navigation-buttons'), 'ModeMain navigation buttons should be visible after navigating to Stock Management').toBeVisible();

    // 2. Receive Stock: MOH supplier, batch BATCH-E2E-001, qty 100.
    await completeReceiveStock(page, {
      supplierValue: 'moh',
      batchNumber: 'BATCH-E2E-001',
      quantity: '100',
      notes: 'E2E test stock receipt',
    });

    // After save, should return to ModeMain.
    await expect(page.locator('.navigation-buttons'), 'ModeMain navigation buttons should be visible after Receive Stock save').toBeVisible();

    // 3. Correct Entry — Subtraction (Error in input, qty 5).
    await completeCorrectEntry(page, {
      entryType: 'substraction',
      reasonText: 'Error in input',
      quantity: '5',
    });

    // After save, should return to ModeMain.
    await expect(page.locator('.navigation-buttons'), 'ModeMain navigation buttons should be visible after Correct Entry (subtraction) save').toBeVisible();

    // 4. Correct Entry — Addition (Error in input, qty 3).
    await completeCorrectEntry(page, {
      entryType: 'addition',
      reasonText: 'Error in input',
      quantity: '3',
    });

    // After save, should return to ModeMain.
    await expect(page.locator('.navigation-buttons'), 'ModeMain navigation buttons should be visible after Correct Entry (addition) save').toBeVisible();

    // 5. View Month Details — verify the table renders.
    await click(
      page.locator('.navigation-buttons button.ui.primary.button', {
        hasText: 'View current month details',
      }),
      page,
    );
    await expect(page.locator('.pane.month-details'), 'Month details pane should be visible').toBeVisible();
    await expect(page.locator('.pane.month-details .row.header'), 'Month details table header row should be visible').toBeVisible();

    // Navigate back to ModeMain.
    await click(page.locator('.link-back'), page);
    await page.locator('.navigation-buttons').waitFor({ timeout: 10000 });

    // 6. Sync to backend.
    await syncAndWait(page);

    // 7. Backend verification: 3 new stock_update nodes created.
    const result = queryStockUpdateNodes('Nyange Health Center', initialCount + 3);

    // Verify total count increased by 3.
    expect(result.count, 'stock_update node count should increase by at least 3').toBeGreaterThanOrEqual(initialCount + 3);

    // Verify type breakdown: +1 receive-supply, +2 correction.
    expect(result.types['receive-supply'], 'receive-supply count should increase by at least 1').toBeGreaterThanOrEqual(initialReceiveSupply + 1);
    expect(result.types['correction'], 'correction count should increase by at least 2').toBeGreaterThanOrEqual(initialCorrection + 2);
  });
});
