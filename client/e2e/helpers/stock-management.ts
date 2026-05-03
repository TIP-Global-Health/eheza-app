import { Page } from '@playwright/test';
import { execSync } from 'child_process';
import { click } from './auth';
import { drushEnv } from './device';
import { WAIT, setDate } from './common';

/**
 * Click "Yes" on the identity confirmation bool input.
 * After confirming, the rest of the form (including the signature pad)
 * becomes visible. We wait for the signature pad JS binding to complete.
 */
async function confirmIdentity(page: Page) {
  await click(
    page.locator('.form-input.yes-no.confirm-identity label', { hasText: 'Yes' }),
    page,
  );
  // Wait for Elm to re-render the form fields and for the JS signature
  // pad to bind via the bindSignaturePad port by waiting for the
  // signature pad canvas to be ready instead of using a fixed timeout.
  await page.locator('#signature-pad canvas').waitFor({ timeout: 10000 });
}

/**
 * Draw a simple stroke on the signature pad canvas, then click Accept.
 * The SignaturePad JS library tracks its own internal state via pointer
 * event listeners — drawing directly on the 2D context won't work because
 * signaturePad.isEmpty() checks internal state. Must use real mouse events.
 */
async function drawAndAcceptSignature(page: Page) {
  const canvas = page.locator('#signature-pad canvas');
  await canvas.waitFor({ timeout: 10000 });

  // Scroll canvas into viewport — mouse events won't register if it's off-screen.
  await canvas.scrollIntoViewIfNeeded();
  await page.waitForTimeout(WAIT.sectionTransition);

  const box = await canvas.boundingBox();
  if (!box) throw new Error('Signature pad canvas has no bounding box');

  // Draw a simple line across the canvas using pointer-like mouse events.
  const startX = box.x + box.width * 0.2;
  const startY = box.y + box.height * 0.3;
  const midX = box.x + box.width * 0.6;
  const midY = box.y + box.height * 0.5;
  const endX = box.x + box.width * 0.4;
  const endY = box.y + box.height * 0.7;

  await page.mouse.move(startX, startY);
  await page.mouse.down();
  await page.mouse.move(midX, midY, { steps: 15 });
  await page.mouse.move(endX, endY, { steps: 15 });
  await page.mouse.up();
  await page.waitForTimeout(WAIT.elmRerender);

  // Click the Accept button.
  const acceptBtn = page.locator('.signature-pad--footer button.primary');
  await acceptBtn.scrollIntoViewIfNeeded();
  await page.waitForTimeout(WAIT.formInteraction);
  await click(acceptBtn, page);

  // Wait for the signature to be stored and the image to appear.
  // The JS storeSignature function converts canvas to blob, stores in cache,
  // then fires a "signaturecomplete" custom event. Elm receives the URL and
  // replaces the canvas with <div class="signature"><img></div>.
  await page.locator('.signature img').waitFor({ timeout: 15000 });
  await page.waitForTimeout(WAIT.elmRerender);
}

/**
 * Click the Save button (must have .active class to be clickable).
 * After save, the Elm model resets to emptyModel (ModeMain).
 */
async function clickSave(page: Page) {
  const saveBtn = page.locator('button.ui.fluid.primary.button.active', { hasText: 'Save' });
  await saveBtn.waitFor({ timeout: 10000 });
  await click(saveBtn, page);

  // After save, Elm resets to ModeMain — wait for navigation buttons.
  await page.locator('.navigation-buttons').waitFor({ timeout: 10000 });
  await page.waitForTimeout(WAIT.sectionTransition);
}

// ---------------------------------------------------------------------------
// Exported helpers
// ---------------------------------------------------------------------------

/**
 * Navigate to Stock Management from the main dashboard.
 * Clicks the "Stock Management" card and waits for the page to render.
 */
export async function navigateToStockManagement(page: Page) {
  // Wait for dashboard cards to be visible.
  await page.locator('.wrap-cards').waitFor({ timeout: 10000 });

  // Click the Stock Management card.
  await click(page.locator('.icon-task-stock-management'), page);

  // Wait for the stock management page to render.
  await page.locator('div.page-activity.stock-management').waitFor({ timeout: 10000 });
  await page.waitForTimeout(WAIT.sectionTransition);
}

/**
 * Complete the "Receive Stock" form.
 *
 * Flow: Click "Receive Stock" → confirm identity → fill date, supplier,
 * batch number, expiration date, quantity, observations → sign → save.
 */
export async function completeReceiveStock(
  page: Page,
  options?: {
    supplierValue?: string;
    batchNumber?: string;
    quantity?: string;
    notes?: string;
  },
) {
  const {
    supplierValue = 'moh',
    batchNumber = 'BATCH-E2E-001',
    quantity = '100',
    notes = 'E2E test stock receipt',
  } = options ?? {};

  // Click "Receive Stock" button.
  await click(
    page.locator('.navigation-buttons button.ui.primary.button', { hasText: 'Receive Stock' }),
    page,
  );
  await page.waitForTimeout(WAIT.sectionTransition);

  // Confirm identity.
  await confirmIdentity(page);

  // Set Date Recorded (first .form-input.date element) — use today.
  const today = new Date();
  const formContent = page.locator('div.ui.full.content');
  await setDate(page, today, 'div.ui.full.content .form-input.date >> nth=0');

  // Select supplier.
  await formContent.locator('select.form-input.select').selectOption(supplierValue);

  // Fill batch number.
  await formContent.locator('input.form-input.batch-number').fill(batchNumber);

  // Set Expiration Date (second .form-input.date element) — 1 year from now.
  const expiryDate = new Date();
  expiryDate.setFullYear(expiryDate.getFullYear() + 1);
  await setDate(page, expiryDate, 'div.ui.full.content .form-input.date >> nth=1');

  // Fill quantity.
  await formContent.locator('.form-input.number.quantity input[type="number"]').fill(quantity);

  // Fill observations.
  await formContent.locator('textarea.form-input.textarea').fill(notes);

  // Draw and accept signature.
  await drawAndAcceptSignature(page);

  // Save.
  await clickSave(page);
}

/**
 * Complete the "Correct Entry" form.
 *
 * Flow: Click "Correct entry" → confirm identity → fill date,
 * correction type, correction reason, quantity → sign → save.
 */
export async function completeCorrectEntry(
  page: Page,
  options?: {
    entryType?: string;
    reasonText?: string;
    quantity?: string;
  },
) {
  const {
    entryType = 'substraction',
    reasonText = 'Error in input',
    quantity = '5',
  } = options ?? {};

  // Click "Correct entry" button.
  await click(
    page.locator('.navigation-buttons button.ui.primary.button', { hasText: 'Correct entry' }),
    page,
  );
  await page.waitForTimeout(WAIT.sectionTransition);

  // Confirm identity.
  await confirmIdentity(page);

  const formContent = page.locator('div.ui.full.content');

  // Set Date — use today (only one .form-input.date on this form).
  const today = new Date();
  await setDate(page, today, 'div.ui.full.content .form-input.date');

  // Select correction type.
  await formContent.locator('select.form-input.correction-type').selectOption(entryType);
  await page.waitForTimeout(WAIT.sectionTransition);

  // Select correction reason (checkbox select, visible after type is chosen).
  const reasonSection = page.locator('.correction-reason:not(.hidden)');
  await reasonSection.waitFor({ timeout: 5000 });
  await click(
    reasonSection.locator('.ui.checkbox.activity', { hasText: reasonText }),
    page,
  );

  // Fill quantity.
  await formContent.locator('.form-input.number.quantity input[type="number"]').fill(quantity);

  // Draw and accept signature.
  await drawAndAcceptSignature(page);

  // Save.
  await clickSave(page);
}

// ---------------------------------------------------------------------------
// Feature flag helpers
// ---------------------------------------------------------------------------

/**
 * Ensure the HC-level stock management feature flag is enabled.
 * Required before a nurse can see the "Stock Management" card on the
 * dashboard. Mirrors `ensureNCDAFeatureEnabled` in `reports.ts`.
 *
 * Note: execSync with a hardcoded command — no user input involved.
 */
export function ensureStockManagementHCFeatureEnabled() {
  const { drushCmd, cwd } = drushEnv();
  execSync(
    `${drushCmd} vset hedley_admin_feature_stock_management_hc_enabled 1`,
    { cwd, timeout: 15000, encoding: 'utf-8', stdio: 'pipe' },
  );
}

/**
 * Ensure the village-level stock management feature flag is enabled.
 * Required before a CHW can see the "Stock Management" card on the
 * dashboard. Mirrors `ensureNCDAFeatureEnabled` in `reports.ts`.
 *
 * Note: execSync with a hardcoded command — no user input involved.
 */
export function ensureStockManagementVillageFeatureEnabled() {
  const { drushCmd, cwd } = drushEnv();
  execSync(
    `${drushCmd} vset hedley_admin_feature_stock_management_village_enabled 1`,
    { cwd, timeout: 15000, encoding: 'utf-8', stdio: 'pipe' },
  );
}

// ---------------------------------------------------------------------------
// Backend verification via drush
// ---------------------------------------------------------------------------

/**
 * Query the backend for stock_update nodes associated with a health center.
 * Returns an object with count and types breakdown.
 *
 * When `villageName` is supplied, results are further filtered to stock
 * updates whose `field_village_ref` points to the village node with that
 * title. This is required for CHW village-level stock tests so that the
 * assertion only counts village-scoped stock updates (not HC ones).
 *
 * Uses delta-based verification: compare against an initial count to avoid
 * interference from existing demo data.
 *
 * Retries up to 10 times with 5s delay for eventual consistency.
 *
 * NOTE: Uses execSync with base64-encoded input to prevent shell injection.
 * This follows the same pattern as other E2E helpers (ncd.ts, hiv.ts).
 */
export function queryStockUpdateNodes(
  healthCenterName: string,
  expectedCount?: number,
  villageName?: string,
): { count: number; types: Record<string, number> } {
  const hcNameB64 = Buffer.from(healthCenterName, 'utf8').toString('base64');
  const villageNameB64 = villageName
    ? Buffer.from(villageName, 'utf8').toString('base64')
    : '';
  const filterByVillage = villageName ? 'true' : 'false';

  const php = `
    \\$hc_name = base64_decode('${hcNameB64}');
    \\$query = new EntityFieldQuery();
    \\$result = \\$query->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'health_center')
      ->propertyCondition('title', \\$hc_name)
      ->execute();
    if (empty(\\$result['node'])) {
      echo json_encode(['error' => 'Health center not found']);
      return;
    }
    \\$hc_nid = key(\\$result['node']);

    \\$filter_by_village = ${filterByVillage};
    \\$village_nid = NULL;
    if (\\$filter_by_village) {
      \\$village_name = base64_decode('${villageNameB64}');
      \\$vq = new EntityFieldQuery();
      \\$vr = \\$vq->entityCondition('entity_type', 'node')
        ->propertyCondition('type', 'village')
        ->fieldCondition('field_village', 'value', \\$village_name)
        ->execute();
      if (empty(\\$vr['node'])) {
        echo json_encode(['error' => 'Village not found']);
        return;
      }
      \\$village_nid = key(\\$vr['node']);
    }

    \\$q = new EntityFieldQuery();
    \\$q->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'stock_update')
      ->fieldCondition('field_health_center', 'target_id', \\$hc_nid);
    if (\\$filter_by_village) {
      \\$q->fieldCondition('field_village_ref', 'target_id', \\$village_nid);
    }
    \\$r = \\$q->execute();

    \\$types = array('receive-supply' => 0, 'correction' => 0);
    \\$count = 0;
    if (!empty(\\$r['node'])) {
      foreach (array_keys(\\$r['node']) as \\$nid) {
        \\$node = node_load(\\$nid);
        \\$update_type = \\$node->field_stock_update_type[LANGUAGE_NONE][0]['value'];
        if (isset(\\$types[\\$update_type])) {
          \\$types[\\$update_type]++;
        }
        \\$count++;
      }
    }
    echo json_encode(array('count' => \\$count, 'types' => \\$types));
  `;

  const { drushCmd, cwd } = drushEnv();

  for (let attempt = 0; attempt < 10; attempt++) {
    try {
      const output = execSync(`${drushCmd} eval "${php}"`, {
        cwd,
        timeout: 30000,
        encoding: 'utf-8',
      }).trim();

      const parsed = JSON.parse(output);
      if (parsed.error) {
        console.log(`queryStockUpdateNodes attempt ${attempt + 1}: ${parsed.error}`);
        if (attempt < 9) {
          execSync('sleep 5');
          continue;
        }
        throw new Error(`queryStockUpdateNodes: ${parsed.error}`);
      }

      // Check if we've reached the expected count.
      if (expectedCount !== undefined) {
        if (parsed.count >= expectedCount) {
          return parsed;
        }
        console.log(
          `queryStockUpdateNodes attempt ${attempt + 1}: found ${parsed.count}, expected ${expectedCount}`,
        );
        if (attempt < 9) {
          execSync('sleep 5');
          continue;
        }
      }

      return parsed;
    } catch (err) {
      console.log(`queryStockUpdateNodes attempt ${attempt + 1}: error`, err);
      if (attempt < 9) {
        execSync('sleep 5');
      }
    }
  }

  return { count: 0, types: { 'receive-supply': 0, correction: 0 } };
}
