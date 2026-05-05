import { Page } from '@playwright/test';
import { execSync } from 'child_process';
import { readFileSync } from 'fs';
import { resolve } from 'path';
import { getClientPort } from './client-port';
import { WAIT } from './common';
import { drushEnv } from './device';

// ---------------------------------------------------------------------------
// DDEV URL resolution
// ---------------------------------------------------------------------------

/**
 * Get the Drupal admin base URL from DDEV.
 *
 * DDEV hostname can be the directory name OR the `name` field in
 * config.yaml depending on the environment. We try the `ddev describe`
 * command first for accuracy, then fall back to config parsing.
 */
export function getDdevUrl(): string {
  const { drushCmd, cwd } = drushEnv();
  // Derive `ddev` command from `ddev drush` → `ddev`.
  // Inside the container, drushCmd is just 'drush' — ddev describe won't work.
  const isDdev = drushCmd.startsWith('ddev');
  const ddevCmd = isDdev ? 'ddev' : null;

  // Try `ddev describe` to get the actual URL (only outside the container).
  if (ddevCmd) try {
    const output = execSync(`${ddevCmd} describe -j`, {
      cwd,
      timeout: 15000,
      encoding: 'utf-8',
      stdio: 'pipe',
    });
    const data = JSON.parse(output);
    // ddev describe -j returns { raw: { ... } } with httpurl/httpsurl fields.
    const raw = data?.raw;
    if (raw?.httpsurl) {
      return raw.httpsurl;
    }
    if (raw?.httpurl) {
      return raw.httpurl;
    }
  } catch {
    // Fall back to config parsing.
  }

  // Fallback: read from config.
  const projectRoot = process.cwd().replace(/\/client$/, '');
  let name = projectRoot.split('/').pop() ?? 'eheza-app';
  let port = '4443';
  const configCandidates = [
    resolve(projectRoot, '.ddev/config.yaml'),
    resolve(__dirname, '../../../.ddev/config.yaml'),
  ];
  for (const configPath of configCandidates) {
    try {
      const content = readFileSync(configPath, 'utf-8');
      const nameMatch = content.match(/^name:\s*(.+)$/m);
      const portMatch = content.match(/^router_https_port:\s*"?(\d+)"?$/m);
      if (nameMatch) name = nameMatch[1].trim();
      if (portMatch) port = portMatch[1];
      break;
    } catch {
      // Try next.
    }
  }

  return `https://${name}.ddev.site:${port}`;
}

// ---------------------------------------------------------------------------
// Drupal authentication
// ---------------------------------------------------------------------------

/**
 * Login to the Drupal admin interface.
 */
export async function drupalLogin(
  page: Page,
  username = 'admin',
  password = 'admin',
) {
  const baseUrl = getDdevUrl();
  await page.goto(`${baseUrl}/user/login`);

  await page.locator('input[name="name"]').fill(username);
  await page.locator('input[name="pass"]').fill(password);
  await page.locator('#edit-submit').click();

  // Wait for redirect — either admin dashboard or the page we came from.
  await page.waitForURL(url => !url.toString().includes('/user/login'), {
    timeout: 15000,
  });
}

// ---------------------------------------------------------------------------
// Report data generation via drush
// ---------------------------------------------------------------------------

/**
 * Generate base reports data for all existing persons.
 * This populates `field_reports_data` on person nodes.
 *
 * Note: execSync is used here following the same pattern as other E2E
 * helpers (device.ts, stock-management.ts) for drush command execution.
 * The input is hardcoded (no user-provided data), so shell injection
 * is not a concern.
 */
/**
 * Ensure hedley_prenatal_change_medications is set.
 * Required for the completion script to use individual medication types
 * (Iron, Folate, etc.) instead of the legacy single 'medication' activity.
 * On fresh installs, hedley_install() sets this, but CI may use a DB dump
 * where the variable is missing.
 */
export function ensurePrenatalMedicationsVariable() {
  const { drushCmd, cwd } = drushEnv();
  const php = 'variable_set("hedley_prenatal_change_medications", "2020-01-01"); echo variable_get("hedley_prenatal_change_medications", "NOT SET");';
  const result = execSync(
    `${drushCmd} eval '${php}'`,
    { cwd, timeout: 15000, encoding: 'utf-8', stdio: 'pipe' },
  ).trim();
  console.log('hedley_prenatal_change_medications:', result);
}

/**
 * Ensure the NCDA feature flag is enabled.
 * Required before generating NCDA person data or running scoreboard tests.
 * The input is hardcoded (no user-provided data), so shell injection
 * is not a concern.
 */
export function ensureNCDAFeatureEnabled() {
  const { drushCmd, cwd } = drushEnv();
  console.log('Enabling NCDA feature flag...');
  execSync(
    `${drushCmd} vset hedley_admin_feature_ncda_enabled 1`,
    { cwd, timeout: 15000, encoding: 'utf-8', stdio: 'pipe' },
  );
  console.log('NCDA feature flag enabled.');
}

/**
 * Generate NCDA person data for all existing persons.
 * This populates NCDA-specific report fields on person nodes.
 * Pass excludeSet=true to add --exclude_set=1 flag.
 *
 * Note: execSync is used here following the same pattern as other E2E
 * helpers for drush command execution. The input is hardcoded
 * (no user-provided data), so shell injection is not a concern.
 */
export function generateNCDAPersonData(excludeSet = false) {
  const { drushCmd, cwd } = drushEnv();
  const flag = excludeSet ? ' --exclude_set=1' : '';
  console.log('Generating NCDA person data (generate-data-for-all.php)...');
  execSync(
    `${drushCmd} scr profiles/hedley/modules/custom/hedley_ncda/scripts/generate-data-for-all.php${flag}`,
    { cwd, timeout: 300000, encoding: 'utf-8', stdio: 'pipe' },
  );
  console.log('NCDA person data generated.');
}

/**
 * Recalculate NCDA large datasets -- aggregates per-person NCDA data into
 * scope-level report_data nodes, then clears Drupal caches.
 */
export function ncdaRecalculateLargeDatasets() {
  const { drushCmd, cwd } = drushEnv();
  console.log('Recalculating NCDA large datasets...');
  execSync(
    `${drushCmd} scr profiles/hedley/modules/custom/hedley_ncda/scripts/recalculate-large-datasets.php`,
    { cwd, timeout: 300000, encoding: 'utf-8', stdio: 'pipe' },
  );
  // Clear Drupal caches so pages serve the fresh report_data.
  execSync(`${drushCmd} cc all`, {
    cwd, timeout: 30000, encoding: 'utf-8', stdio: 'pipe',
  });
  console.log('NCDA large datasets recalculated.');
}

/**
 * Backdate a person node's `created` timestamp.
 *
 * The NCDA scoreboard Elm view uses a strict less-than check
 * (Date.compare record.created targetDateForMonth == LT) to determine
 * whether a child existed during an examination month. Children created
 * on the same day as the target date are excluded. This helper sets
 * the created timestamp to `monthsAgo` months in the past so that
 * test-created children appear in the scoreboard.
 *
 * Uses db_update (not node_save) to avoid triggering hooks.
 *
 * Note: execSync with base64-encoded person name — same pattern as
 * backdateNutritionEncounter(). The name is base64-encoded to prevent
 * shell metacharacter issues (not user-provided data).
 */
export function backdatePersonCreated(personName: string, monthsAgo = 2) {
  const { drushCmd, cwd } = drushEnv();
  const personNameB64 = Buffer.from(personName, 'utf8').toString('base64');
  const target = new Date();
  target.setMonth(target.getMonth() - monthsAgo);
  const timestamp = Math.floor(target.getTime() / 1000);

  const php = `
    \\$name = base64_decode('${personNameB64}');
    \\$q = new EntityFieldQuery();
    \\$r = \\$q->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'person')
      ->propertyCondition('title', \\$name)
      ->execute();
    if (empty(\\$r['node'])) { echo 'NOT FOUND'; return; }
    \\$nid = key(\\$r['node']);
    db_update('node')
      ->fields(['created' => ${timestamp}])
      ->condition('nid', \\$nid)
      ->execute();
    echo \\$nid;
  `;

  const result = execSync(`${drushCmd} eval "${php}"`, {
    cwd, timeout: 30000, encoding: 'utf-8', stdio: 'pipe',
  }).trim();
  console.log(`Backdated person "${personName}": nid=${result}`);
}

// ---------------------------------------------------------------------------
// NCDA Scoreboard navigation and table reading helpers
// ---------------------------------------------------------------------------

export interface ScoreboardRow {
  label: string;
  values: string[];
}

export interface ScoreboardPaneData {
  heading: string;
  rows: ScoreboardRow[];
}

/**
 * Navigate to the aggregated NCDA scoreboard page for a given geo path.
 * Retries up to 5 times if panes are not yet rendered (handles lazy data fetch).
 *
 * Pane indices: 0=entity info, 1=demographics, 2=acute malnutrition,
 * 3=stunting, 4=ANC/newborn, 5=universal interventions,
 * 6=nutrition behavior, 7=targeted interventions, 8=infrastructure/WASH.
 */
export async function navigateToNCDAScoreboard(page: Page, geoPath: string): Promise<void> {
  // Append cache-busting query parameter to avoid Drupal serving stale pages.
  const cacheBuster = Date.now();
  const url = `${getDdevUrl()}/admin/reports/aggregated-ncda/${geoPath}?t=${cacheBuster}`;
  console.log(`Navigating to NCDA scoreboard: ${url}`);
  await page.goto(url);

  let paneCount = 0;
  for (let attempt = 0; attempt < 5; attempt++) {
    await page.waitForTimeout(WAIT.pageNavigation);
    paneCount = await page.locator('.pane').count();
    if (paneCount >= 2) {
      break;
    }
    await page.reload();
  }

  if (paneCount < 2) {
    throw new Error(`NCDA scoreboard at ${url} did not render at least 2 panes after 5 retries (got ${paneCount})`);
  }

  await page.waitForTimeout(WAIT.sectionTransition);
}

/**
 * Read the content of a scoreboard pane identified by heading text.
 * More robust than index-based lookup — survives pane reordering.
 */
export async function readScoreboardPane(page: Page, headingText: string): Promise<ScoreboardPaneData> {
  const pane = page.locator('div.pane').filter({
    has: page.locator('.pane-heading', { hasText: headingText }),
  });
  await pane.waitFor({ timeout: 10000 });
  const heading = (await pane.locator('.pane-heading').textContent())?.trim() ?? '';

  const tableRows = pane.locator('.table-row');
  const rowCount = await tableRows.count();

  const rows: ScoreboardRow[] = [];
  for (let i = 0; i < rowCount; i++) {
    const row = tableRows.nth(i);
    const label = await row.locator('.cell.activity').innerText();
    const valueCells = row.locator('.cell.value');
    const valueCount = await valueCells.count();
    const values: string[] = [];
    for (let j = 0; j < valueCount; j++) {
      values.push(await valueCells.nth(j).innerText());
    }
    rows.push({ label, values });
  }

  return { heading, rows };
}

/**
 * Get the 0-based column index for the current month (UTC).
 * The scoreboard renders Jan=0, Feb=1, ..., Dec=11.
 * Uses UTC because the Elm app derives currentDate via Time.utc.
 */
export function getCurrentMonthColumnIndex(): number {
  return new Date().getUTCMonth();
}

/**
 * Find a value in a scoreboard pane by row label substring and month index.
 * Month index is 0-based (Jan=0). Throws if the row or cell is missing, or
 * if the value is not a valid integer. Returns 0 only for an existing empty cell.
 */
export function findScoreboardValue(pane: ScoreboardPaneData, rowLabel: string, monthIndex: number): number {
  const row = pane.rows.find(r => r.label.includes(rowLabel));
  if (!row) {
    throw new Error(
      `Scoreboard row not found for label "${rowLabel}" in pane "${pane.heading}". Available rows: ${pane.rows
        .map(r => `"${r.label}"`)
        .join(', ')}`,
    );
  }

  const raw = row.values[monthIndex];
  if (raw === undefined) {
    throw new Error(
      `Scoreboard value not found for row "${row.label}" at month index ${monthIndex} in pane "${pane.heading}".`,
    );
  }

  const trimmed = raw.trim();
  if (trimmed === '') {
    return 0;
  }

  const parsed = parseInt(trimmed, 10);
  if (isNaN(parsed)) {
    throw new Error(
      `Invalid scoreboard value "${raw}" for row "${row.label}" at month index ${monthIndex} in pane "${pane.heading}".`,
    );
  }

  return parsed;
}

export function generateBaseReportsData() {
  const { drushCmd, cwd } = drushEnv();
  console.log('Generating base reports data (generate-data-for-all.php)...');
  execSync(
    `${drushCmd} scr profiles/hedley/modules/custom/hedley_reports/scripts/generate-data-for-all.php`,
    { cwd, timeout: 300000, encoding: 'utf-8', stdio: 'pipe' },
  );
  console.log('Base reports data generated.');
}

/**
 * Delete all pending Advanced Queue items.
 * Call this before creating test encounters to ensure only
 * the items triggered by our test data get processed.
 */
export function clearAdvancedQueue() {
  const { drushCmd, cwd } = drushEnv();
  console.log('Clearing Advanced Queue...');
  const php = `
    db_delete('advancedqueue')
      ->condition('status', array(-1, 0, 1), 'IN')
      ->execute();
    echo 'AQ cleared';
  `;
  execSync(`${drushCmd} eval "${php}"`, {
    cwd,
    timeout: 15000,
    encoding: 'utf-8',
    stdio: 'pipe',
  });
  console.log('Advanced Queue cleared.');
}

/**
 * Process all Advanced Queue items. After syncing new encounters,
 * the backend queues recalculation tasks for affected persons.
 */
export function processAdvancedQueue() {
  const { drushCmd, cwd } = drushEnv();
  console.log('Processing Advanced Queue items...');
  execSync(`${drushCmd} advancedqueue --all --timeout=30`, {
    cwd,
    timeout: 60000,
    encoding: 'utf-8',
    stdio: 'pipe',
  });
  console.log('Advanced Queue processing complete.');
}

/**
 * Recalculate large datasets — aggregates per-person data into
 * scope-level report_data nodes (global, province, district, HC).
 */
export function recalculateLargeDatasets() {
  const { drushCmd, cwd } = drushEnv();
  console.log('Recalculating large datasets...');
  execSync(
    `${drushCmd} scr profiles/hedley/modules/custom/hedley_reports/scripts/recalculate-large-datasets.php`,
    { cwd, timeout: 300000, encoding: 'utf-8', stdio: 'pipe' },
  );
  // Clear Drupal caches so pages serve the fresh report_data.
  execSync(`${drushCmd} cc all`, {
    cwd, timeout: 30000, encoding: 'utf-8', stdio: 'pipe',
  });
  console.log('Large datasets recalculated.');
}

// ---------------------------------------------------------------------------
// Navigation
// ---------------------------------------------------------------------------

/**
 * Navigate to the Statistical Queries results page for a Health Center scope.
 */
export async function navigateToHCReportsPage(
  page: Page,
  healthCenterId: number,
) {
  const baseUrl = getDdevUrl();
  // Cache-busting parameter to prevent browser from serving stale page.
  await page.goto(
    `${baseUrl}/admin/reports/statistical-queries/health-center/${healthCenterId}?t=${Date.now()}`,
  );
  await page.locator('.page-content.reports').waitFor({ timeout: 30000 });
}

/**
 * Navigate to the Statistical Queries menu page.
 */
export async function navigateToReportsMenu(page: Page) {
  const baseUrl = getDdevUrl();
  await page.goto(`${baseUrl}/admin/reports/statistical-queries`);
  await page.locator('.page-content.reports-menu').waitFor({ timeout: 30000 });
}

/**
 * Navigate to the Statistical Queries results page for the Global scope (all
 * patients across all health centers). The Drupal route is registered at
 * `admin/reports/statistical-queries/all` (see hedley_reports.module).
 */
export async function navigateToGlobalReportsPage(page: Page) {
  const baseUrl = getDdevUrl();
  // Cache-busting parameter to prevent browser from serving stale page.
  await page.goto(
    `${baseUrl}/admin/reports/statistical-queries/all?t=${Date.now()}`,
  );
  await page.locator('.page-content.reports').waitFor({ timeout: 30000 });
}

// ---------------------------------------------------------------------------
// Report type & date selection
// ---------------------------------------------------------------------------

/**
 * Select a report type from the dropdown on the results page.
 * Values: "demographics", "acute-illness", "prenatal", "prenatal-diagnoses", "nutrition"
 */
export async function selectReportType(page: Page, reportType: string) {
  const select = page
    .locator('.page-content.reports .select-input-wrapper')
    .first()
    .locator('select.select-input');
  await select.selectOption(reportType);
  // Wait for the report to render.
  await page.waitForTimeout(WAIT.sectionTransition);
}

/**
 * Set the date range (start date and limit date) using the calendar popups.
 * The reports page shows date inputs only after a report type is selected
 * (and not for Nutrition reports).
 */
export async function setDateRange(
  page: Page,
  startDate: Date,
  limitDate: Date,
) {
  // Click first date input (Start Date).
  const dateInputs = page.locator('.page-content.reports div.form-input.date');
  await dateInputs.nth(0).click();
  await selectDateInCalendar(page, startDate);

  // Wait for limit date input to appear.
  await page.waitForTimeout(WAIT.elmRerender);

  // Click second date input (Limit Date).
  await dateInputs.nth(1).click();
  await selectDateInCalendar(page, limitDate);

  // Wait for report content to render with the filtered data.
  await page.waitForTimeout(WAIT.pageNavigation);
}

/**
 * Select a date in the calendar popup.
 */
async function selectDateInCalendar(page: Page, date: Date) {
  const popup = page.locator('.ui.active.modal.calendar-popup');
  await popup.waitFor({ timeout: 5000 });

  // Use UTC — Elm date pickers derive dates via Time.utc.
  // Select year.
  await popup
    .locator('div.calendar > div.year > select')
    .selectOption(date.getUTCFullYear().toString());

  // Select month (1-indexed).
  await popup
    .locator('div.calendar > div.month > select')
    .selectOption((date.getUTCMonth() + 1).toString());

  // Click day.
  const day = date.getUTCDate();
  const dayCell = popup.locator(
    'div.calendar table tbody td:not(.date-selector--dimmed)',
    { hasText: new RegExp(`^${day}$`) },
  );
  await dayCell.first().click();

  // Click Save button in calendar popup.
  await popup.locator('div.ui.button').click();

  // Wait for popup to close.
  await popup.waitFor({ state: 'hidden', timeout: 3000 }).catch(() => {});
}

// ---------------------------------------------------------------------------
// Table reading helpers
// ---------------------------------------------------------------------------

export interface PatientsTableRow {
  label: string;
  male: number;
  female: number;
}

export interface PatientsTableData {
  rows: PatientsTableRow[];
  total: number;
}

/**
 * Read the Registered Patients table from the Demographics report.
 * Table selector: div.table.registered
 * Each row has 3 cells: label, male count, female count.
 */
export async function readRegisteredPatientsTable(
  page: Page,
): Promise<PatientsTableData> {
  return readPatientsTable(page, 'div.report.demographics div.table.registered');
}

/**
 * Read the Impacted Patients table from the Demographics report.
 * Table selector: div.table.impacted
 */
export async function readImpactedPatientsTable(
  page: Page,
): Promise<PatientsTableData> {
  return readPatientsTable(page, 'div.report.demographics div.table.impacted');
}

/**
 * Generic reader for patients tables (registered or impacted).
 * Rows have 3 cells: [label, male, female].
 */
async function readPatientsTable(
  page: Page,
  tableSelector: string,
): Promise<PatientsTableData> {
  const table = page.locator(tableSelector);
  await table.waitFor({ timeout: 10000 });

  // Data rows (skip captions row).
  const dataRows = table.locator('div.row:not(.captions)');
  const count = await dataRows.count();

  const rows: PatientsTableRow[] = [];
  for (let i = 0; i < count; i++) {
    const cells = dataRows.nth(i).locator('div.item');
    const cellTexts: string[] = [];
    const cellCount = await cells.count();
    for (let j = 0; j < cellCount; j++) {
      cellTexts.push((await cells.nth(j).textContent()) ?? '');
    }
    if (cellTexts.length >= 3) {
      rows.push({
        label: cellTexts[0].trim(),
        male: parseInt(cellTexts[1].trim(), 10) || 0,
        female: parseInt(cellTexts[2].trim(), 10) || 0,
      });
    }
  }

  // Total is the sum of all male + female across all rows.
  const total = rows.reduce((sum, r) => sum + r.male + r.female, 0);

  return { rows, total };
}

export interface EncountersTableRow {
  label: string;
  all: number;
  unique: number;
}

export interface EncountersTableData {
  rows: EncountersTableRow[];
  totalAll: number;
  totalUnique: number;
}

/**
 * Read the Encounters table from the Demographics report.
 * The encounters section is rendered after the patients tables.
 * Each row has 3 cells: [encounter type label, all count, unique count].
 *
 * The encounters table is the third div.table in the demographics report
 * (after registered and impacted).
 */
export async function readEncountersTable(
  page: Page,
): Promise<EncountersTableData> {
  const report = page.locator('div.report.demographics');

  // Tables: 0=registered, 1=impacted, 2=encounters
  const tables = report.locator('div.table');
  const tableCount = await tables.count();

  if (tableCount < 3) {
    return { rows: [], totalAll: 0, totalUnique: 0 };
  }

  const encountersTable = tables.nth(2);
  const dataRows = encountersTable.locator('div.row:not(.captions):not(.encounters-totals)');
  const count = await dataRows.count();

  const rows: EncountersTableRow[] = [];
  for (let i = 0; i < count; i++) {
    const cells = dataRows.nth(i).locator('div.item');
    const cellTexts: string[] = [];
    const cellCount = await cells.count();
    for (let j = 0; j < cellCount; j++) {
      cellTexts.push((await cells.nth(j).textContent()) ?? '');
    }
    if (cellTexts.length >= 3) {
      rows.push({
        label: cellTexts[0].trim(),
        all: parseInt(cellTexts[1].trim(), 10) || 0,
        unique: parseInt(cellTexts[2].trim(), 10) || 0,
      });
    }
  }

  // Read totals from the encounters-totals row.
  const totalsRow = encountersTable.locator('div.row.encounters-totals');
  let totalAll = 0;
  let totalUnique = 0;
  if ((await totalsRow.count()) > 0) {
    const totalCells = totalsRow.locator('div.item');
    const totalCellCount = await totalCells.count();
    if (totalCellCount >= 3) {
      totalAll = parseInt((await totalCells.nth(1).textContent()) ?? '0', 10) || 0;
      totalUnique = parseInt((await totalCells.nth(2).textContent()) ?? '0', 10) || 0;
    }
  }

  return { rows, totalAll, totalUnique };
}

/**
 * Find a row in a patients table by its label.
 */
export function findRow(
  data: PatientsTableData,
  label: string,
): PatientsTableRow | undefined {
  return data.rows.find(r => r.label === label);
}

/**
 * Find a row in the encounters table by label text (partial match).
 */
export function findEncounterRow(
  data: EncountersTableData,
  labelSubstring: string,
): EncountersTableRow | undefined {
  return data.rows.find(r => r.label.includes(labelSubstring));
}

// ---------------------------------------------------------------------------
// Acute Illness report table
// ---------------------------------------------------------------------------

export interface SimpleTableRow {
  label: string;
  total: number;
  /**
   * Per-row unit string (e.g., "package", "kg") when the table renders a
   * Unit column. Currently only the FBF Distribution report uses this.
   */
  unit?: string;
  /**
   * Count of distribution events when the table renders an Occurrences
   * column. Currently only the FBF Distribution report uses this.
   */
  occurrences?: number;
}

export interface SimpleTableData {
  rows: SimpleTableRow[];
}

/**
 * Read the Acute Illness report table.
 * Container: div.report.acute-illness div.table
 * Each row has 2 cells: [diagnosis label, total count].
 * Rows have CSS class identifiers (e.g., diagnosis-malaria-uncomplicated, totals).
 */
export async function readAcuteIllnessTable(
  page: Page,
): Promise<SimpleTableData> {
  return readSimpleTable(page, 'div.report.acute-illness div.table');
}

/**
 * Read the Peripartum report table.
 * Container: div.report.peripartum div.table
 * Each row has 2 cells: [outcome / indicator label, total count].
 * Use findSimpleRow() to look up specific rows by label substring.
 */
export async function readPeripartumTable(
  page: Page,
): Promise<SimpleTableData> {
  return readSimpleTable(page, 'div.report.peripartum div.table');
}

/**
 * Read the Postnatal Care report table (PR #1556).
 * Container: div.report.postnatal-care div.table
 * Each row has 2 cells: [age-bucket / SPV-window label, total count].
 * Use findSimpleRow() to look up specific rows by label substring.
 */
export async function readPostnatalCareTable(
  page: Page,
): Promise<SimpleTableData> {
  return readSimpleTable(page, 'div.report.postnatal-care div.table');
}

/**
 * Read the FBF Distribution report table.
 * Container: div.report.fbf-distribution div.table
 * Each row has 4 cells: [category label, total amount, unit, occurrences].
 * Categories: "FBF Child", "FBF Mother", "Aheza Child", "Aheza Mother".
 * Totals are numeric (whole numbers print without decimals; fractional with
 * up to 2dp). Unit is "package" for FBF rows and "kg" for Aheza rows.
 * Occurrences is the count of distribution events contributing to that
 * total (one event per measurement record).
 */
export async function readFBFDistributionTable(
  page: Page,
): Promise<SimpleTableData> {
  return readSimpleTable(page, 'div.report.fbf-distribution div.table');
}

/**
 * Read a specific AI diagnosis row by its CSS class.
 * Returns the total count, or 0 if the row is not found.
 */
export async function readAIDiagnosisRow(
  page: Page,
  cssClass: string,
): Promise<number> {
  const row = page.locator(`div.report.acute-illness div.table div.row.${cssClass}`);
  if (!(await row.isVisible({ timeout: 2000 }).catch(() => false))) {
    return 0;
  }
  const cells = row.locator('div.item');
  const count = await cells.count();
  if (count < 2) return 0;
  const text = (await cells.nth(1).textContent()) ?? '0';
  return parseInt(text.trim(), 10) || 0;
}

/**
 * Generic reader for tables with 2 columns: [label, total].
 */
async function readSimpleTable(
  page: Page,
  tableSelector: string,
): Promise<SimpleTableData> {
  const table = page.locator(tableSelector);
  await table.waitFor({ timeout: 10000 });

  const dataRows = table.locator('div.row:not(.captions)');
  const count = await dataRows.count();

  const rows: SimpleTableRow[] = [];
  for (let i = 0; i < count; i++) {
    const cells = dataRows.nth(i).locator('div.item');
    const cellTexts: string[] = [];
    const cellCount = await cells.count();
    for (let j = 0; j < cellCount; j++) {
      cellTexts.push((await cells.nth(j).textContent()) ?? '');
    }
    if (cellTexts.length >= 2) {
      const row: SimpleTableRow = {
        label: cellTexts[0].trim(),
        total: parseFloat(cellTexts[1].trim()) || 0,
      };
      // 4-cell layout: [label, total, unit, occurrences] (FBF Distribution).
      if (cellTexts.length >= 4) {
        row.unit = cellTexts[2].trim();
        row.occurrences = parseFloat(cellTexts[3].trim()) || 0;
      }
      rows.push(row);
    }
  }

  return { rows };
}

/**
 * Find a row in a simple table by label text (exact or partial match).
 */
export function findSimpleRow(
  data: SimpleTableData,
  label: string,
): SimpleTableRow | undefined {
  return data.rows.find(r => r.label === label)
    || data.rows.find(r => r.label.includes(label));
}

// ---------------------------------------------------------------------------
// Prenatal (ANC) report tables
// ---------------------------------------------------------------------------

export interface PrenatalVisitsRow {
  label: string;
  chw: number;
  hc: number;
  all: number;
}

/**
 * Read a prenatal visits table by its CSS class.
 * Tables: all-pregnancies, active-pregnancies, completed-pregnancies.
 * Each row has 4 cells: [label, CHW, HC, All].
 */
export async function readPrenatalVisitsTable(
  page: Page,
  tableClass: string,
): Promise<PrenatalVisitsRow[]> {
  const table = page.locator(`div.report.prenatal div.table.${tableClass}`);
  await table.waitFor({ timeout: 10000 });

  const dataRows = table.locator('div.row:not(.captions)');
  const count = await dataRows.count();

  const rows: PrenatalVisitsRow[] = [];
  for (let i = 0; i < count; i++) {
    const cells = dataRows.nth(i).locator('div.item');
    const cellTexts: string[] = [];
    const cellCount = await cells.count();
    for (let j = 0; j < cellCount; j++) {
      cellTexts.push((await cells.nth(j).textContent()) ?? '');
    }
    if (cellTexts.length >= 4) {
      rows.push({
        label: cellTexts[0].trim(),
        chw: parseInt(cellTexts[1].trim(), 10) || 0,
        hc: parseInt(cellTexts[2].trim(), 10) || 0,
        all: parseInt(cellTexts[3].trim(), 10) || 0,
      });
    }
  }

  return rows;
}

/**
 * Find a row in a prenatal visits table by label.
 */
export function findPrenatalRow(
  rows: PrenatalVisitsRow[],
  label: string,
): PrenatalVisitsRow | undefined {
  return rows.find(r => r.label === label)
    || rows.find(r => r.label.includes(label));
}

/**
 * Read a specific prenatal diagnosis row by its CSS class.
 */
export async function readPrenatalDiagnosisRow(
  page: Page,
  cssClass: string,
): Promise<number> {
  const row = page.locator(`div.report.prenatal-diagnoses div.table div.row.${cssClass}`);
  if (!(await row.isVisible({ timeout: 2000 }).catch(() => false))) {
    return 0;
  }
  const cells = row.locator('div.item');
  const count = await cells.count();
  if (count < 2) return 0;
  const text = (await cells.nth(1).textContent()) ?? '0';
  return parseInt(text.trim(), 10) || 0;
}

/**
 * Navigate back to the PWA dashboard from any page.
 * Used after starting encounters without completing activities
 * (encounters can only be ended after all mandatory activities
 * are completed, so we just navigate away).
 */
export async function goToDashboard(page: Page) {
  const pwaBaseUrl = `http://localhost:${getClientPort()}`;
  await page.goto(pwaBaseUrl);
  await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
}

// ---------------------------------------------------------------------------
// Backdating helpers
// ---------------------------------------------------------------------------

/**
 * Backdate a person's nutrition encounter and measurement nodes so they
 * appear in the nutrition report (which only shows completed months).
 *
 * Updates:
 *   - `field_scheduled_date` on the nutrition_encounter node
 *   - `field_date_measured` on all measurement nodes linked to the person
 *
 * @param personName - Person title as stored in Drupal ("SecondName FirstName")
 * @param targetDate - The date to set (e.g., 1 month ago)
 *
 * Note: execSync is used here following the same pattern as other E2E
 * helpers (device.ts, reports.ts) for drush command execution.
 * The input is base64-encoded (no user-provided data in the shell
 * command), so shell injection is not a concern.
 */
export function backdateNutritionEncounter(
  personName: string,
  targetDate: Date,
) {
  const { drushCmd, cwd } = drushEnv();
  const personNameB64 = Buffer.from(personName, 'utf8').toString('base64');
  const dateStr = targetDate.toISOString().split('T')[0]; // YYYY-MM-DD

  const php = `
    \\$person_name = base64_decode('${personNameB64}');
    \\$date_str = '${dateStr}';

    // Find person.
    \\$query = new EntityFieldQuery();
    \\$result = \\$query->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'person')
      ->propertyCondition('title', \\$person_name)
      ->execute();
    if (empty(\\$result['node'])) {
      echo json_encode(['error' => 'Person not found: ' . \\$person_name]);
      return;
    }
    \\$person_nid = key(\\$result['node']);
    \\$updated = [];

    // Find individual_participant for this person (nutrition program).
    \\$q = new EntityFieldQuery();
    \\$r = \\$q->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'individual_participant')
      ->fieldCondition('field_person', 'target_id', \\$person_nid)
      ->execute();
    if (!empty(\\$r['node'])) {
      foreach (array_keys(\\$r['node']) as \\$participant_nid) {
        // Find nutrition_encounter linked to this participant.
        \\$eq = new EntityFieldQuery();
        \\$er = \\$eq->entityCondition('entity_type', 'node')
          ->propertyCondition('type', 'nutrition_encounter')
          ->fieldCondition('field_individual_participant', 'target_id', \\$participant_nid)
          ->execute();
        if (!empty(\\$er['node'])) {
          foreach (array_keys(\\$er['node']) as \\$nid) {
            \\$node = node_load(\\$nid);
            \\$node->field_scheduled_date[LANGUAGE_NONE][0]['value'] = \\$date_str;
            node_save(\\$node);
            \\$updated[] = 'encounter:' . \\$nid;
          }
        }
      }
    }

    // Update measurement nodes: field_date_measured.
    \\$types = ['nutrition_height', 'nutrition_weight', 'nutrition_muac', 'nutrition_nutrition'];
    foreach (\\$types as \\$type) {
      \\$q = new EntityFieldQuery();
      \\$r = \\$q->entityCondition('entity_type', 'node')
        ->propertyCondition('type', \\$type)
        ->fieldCondition('field_person', 'target_id', \\$person_nid)
        ->execute();
      if (!empty(\\$r['node'])) {
        foreach (array_keys(\\$r['node']) as \\$nid) {
          \\$node = node_load(\\$nid);
          \\$node->field_date_measured[LANGUAGE_NONE][0]['value'] = \\$date_str;
          node_save(\\$node);
          \\$updated[] = \\$type . ':' . \\$nid;
        }
      }
    }

    echo json_encode(['updated' => \\$updated]);
  `;

  console.log(`Backdating nutrition data for "${personName}" to ${dateStr}...`);
  execSync(`${drushCmd} eval "${php}"`, {
    cwd,
    timeout: 30000,
    encoding: 'utf-8',
    stdio: 'pipe',
  });
  console.log('Backdated successfully.');
}

/**
 * Backdate a family-nutrition encounter and its child MUAC measurements
 * for a specific child.
 *
 * Family-nutrition data is stored on the child side under the
 * 'family-nutrition-muac' key in field_reports_data. The query walks
 * family_nutrition_muac_child measurements where field_person == child,
 * then walks each measurement to its parent family_nutrition_encounter.
 * Both the encounter's field_scheduled_date and the measurement's
 * field_date_measured are updated -- the encounter date is what the
 * reports pipeline reads; the measurement date is updated for
 * consistency.
 *
 * Same drush eval pattern as backdateNutritionEncounter; child name is
 * base64-encoded into the PHP body to avoid any shell metacharacter
 * issues with the patient's actual name.
 */
export function backdateFamilyNutritionEncounter(
  childName: string,
  targetDate: Date,
) {
  const { drushCmd, cwd } = drushEnv();
  const childNameB64 = Buffer.from(childName, 'utf8').toString('base64');
  const dateStr = targetDate.toISOString().split('T')[0];

  const php = `
    \\$child_name = base64_decode('${childNameB64}');
    \\$date_str = '${dateStr}';

    // Find child person.
    \\$query = new EntityFieldQuery();
    \\$result = \\$query->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'person')
      ->propertyCondition('title', \\$child_name)
      ->execute();
    if (empty(\\$result['node'])) {
      echo json_encode(['error' => 'Child not found: ' . \\$child_name]);
      return;
    }
    \\$child_nid = key(\\$result['node']);
    \\$updated = [];

    // Find family_nutrition_muac_child measurements for this child.
    \\$mq = new EntityFieldQuery();
    \\$mr = \\$mq->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'family_nutrition_muac_child')
      ->fieldCondition('field_person', 'target_id', \\$child_nid)
      ->execute();
    if (empty(\\$mr['node'])) {
      echo json_encode(['error' => 'No family-nutrition MUAC measurements for child: ' . \\$child_name]);
      return;
    }

    // Update each measurement's field_date_measured AND collect the
    // distinct parent encounter ids to update their field_scheduled_date.
    \\$encounter_ids = [];
    foreach (array_keys(\\$mr['node']) as \\$mid) {
      \\$muac_node = node_load(\\$mid);
      \\$muac_node->field_date_measured[LANGUAGE_NONE][0]['value'] = \\$date_str;
      node_save(\\$muac_node);
      \\$updated[] = 'family_nutrition_muac_child:' . \\$mid;
      \\$eid = \\$muac_node->field_family_nutrition_encounter[LANGUAGE_NONE][0]['target_id'];
      if (!empty(\\$eid)) {
        \\$encounter_ids[\\$eid] = TRUE;
      }
    }

    foreach (array_keys(\\$encounter_ids) as \\$eid) {
      \\$enc_node = node_load(\\$eid);
      \\$enc_node->field_scheduled_date[LANGUAGE_NONE][0]['value'] = \\$date_str;
      node_save(\\$enc_node);
      \\$updated[] = 'family_nutrition_encounter:' . \\$eid;
    }

    echo json_encode(['updated' => \\$updated]);
  `;

  const command = `${drushCmd} eval "${php}"`;
  console.log(`Backdating family nutrition data for "${childName}" to ${dateStr}...`);
  execSync(command, {
    cwd,
    timeout: 30000,
    encoding: 'utf-8',
    stdio: 'pipe',
  });
  console.log('Backdated successfully.');
}

// ---------------------------------------------------------------------------
// Nutrition report table
// ---------------------------------------------------------------------------

export interface NutritionMetricRow {
  label: string;
  values: number[]; // all column values (reverse chronological: newest first)
}

/**
 * The 8 nutrition tables alternate One Visit / Two Visits:
 *   0 = Prevalence By Month (One Visit Or More)
 *   1 = Prevalence By Month (Two Visits Or More)
 *   2 = Incidence By Month  (One Visit Or More)
 *   3 = Incidence By Month  (Two Visits Or More)
 *   4 = Incidence By Quarter (One Visit Or More)
 *   5 = Incidence By Quarter (Two Visits Or More)
 *   6 = Incidence By Year   (One Visit Or More)
 *   7 = Incidence By Year   (Two Visits Or More)
 *
 * "One Visit Or More" tables are at even indices: 0, 2, 4, 6.
 * Columns are in reverse chronological order (newest first).
 * The current month is NOT included — only completed months.
 */
export const NUTRITION_ONE_VISIT_TABLES = [
  { index: 0, name: 'Prevalence By Month' },
  { index: 2, name: 'Incidence By Month' },
  { index: 4, name: 'Incidence By Quarter' },
  { index: 6, name: 'Incidence By Year' },
] as const;

/**
 * "Two Visits Or More" tables are at odd indices: 1, 3, 5, 7. They use
 * `encountersByMonthForImpacted` and only count children with more than
 * one nutrition encounter.
 */
export const NUTRITION_TWO_VISIT_TABLES = [
  { index: 1, name: 'Prevalence By Month (Two Visits Or More)' },
  { index: 3, name: 'Incidence By Month (Two Visits Or More)' },
  { index: 5, name: 'Incidence By Quarter (Two Visits Or More)' },
  { index: 7, name: 'Incidence By Year (Two Visits Or More)' },
] as const;

/**
 * Read a nutrition table by its 0-based index.
 * Each data row has: [metric label, value1%, value2%, ...].
 * Columns are reverse chronological (newest first).
 * The header row has no .captions class — it's skipped by the
 * empty row-label check.
 */
export async function readNutritionTable(
  page: Page,
  tableIndex: number,
): Promise<NutritionMetricRow[]> {
  const table = page.locator('div.report.nutrition div.table.wide').nth(tableIndex);
  await table.waitFor({ timeout: 10000 });

  const allRows = table.locator('div.row');
  const count = await allRows.count();
  const rows: NutritionMetricRow[] = [];

  for (let i = 0; i < count; i++) {
    const labelEl = allRows.nth(i).locator('div.item.row-label');
    const label = (await labelEl.textContent())?.trim() ?? '';
    if (!label) continue; // skip header row (empty row-label)

    const valueCells = allRows.nth(i).locator('div.item.value');
    const valCount = await valueCells.count();
    const values: number[] = [];
    for (let j = 0; j < valCount; j++) {
      const text = (await valueCells.nth(j).textContent()) ?? '0';
      values.push(parseFloat(text.replace('%', '').trim()) || 0);
    }
    rows.push({ label, values });
  }
  return rows;
}

/**
 * Read the column headers from a nutrition table.
 * Returns labels in display order (reverse chronological: newest first).
 */
export async function readNutritionColumnHeaders(
  page: Page,
  tableIndex: number,
): Promise<string[]> {
  const table = page.locator('div.report.nutrition div.table.wide').nth(tableIndex);
  await table.waitFor({ timeout: 10000 });

  // Header row is the first div.row. Its cells after the row-label
  // have class "heading".
  const headerRow = table.locator('div.row').first();
  const headings = headerRow.locator('div.item.heading');
  const count = await headings.count();
  const labels: string[] = [];
  for (let i = 0; i < count; i++) {
    labels.push((await headings.nth(i).textContent())?.trim() ?? '');
  }
  return labels;
}

/**
 * Find a nutrition metric row by label substring.
 */
export function findNutritionMetric(
  rows: NutritionMetricRow[],
  labelSubstring: string,
): NutritionMetricRow | undefined {
  return rows.find(r => r.label.includes(labelSubstring));
}

// ---------------------------------------------------------------------------
// Completion Report helpers
// ---------------------------------------------------------------------------

/**
 * Generate per-encounter completion data for a specific encounter type.
 * Runs the Layer 1 PHP script that populates `field_reports_data` on
 * encounter nodes with completion strings.
 *
 * Note: execSync is used here following the same pattern as other E2E
 * helpers (generateBaseReportsData, recalculateLargeDatasets) for drush
 * command execution. The input is hardcoded (no user-provided data),
 * so shell injection is not a concern.
 *
 * @param encounterType - e.g., 'acute-illness', 'prenatal', 'ncd'
 * @param excludeSet - if true, skip encounters that already have completion data
 */
export function generateCompletionData(encounterType: string, excludeSet = false) {
  const { drushCmd, cwd } = drushEnv();
  const excludeFlag = excludeSet ? ' --exclude_set=1' : '';
  console.log(`Generating completion data (completion-generate-${encounterType}-data.php${excludeFlag})...`);
  execSync(
    `${drushCmd} scr profiles/hedley/modules/custom/hedley_reports/scripts/completion-generate-${encounterType}-data.php${excludeFlag}`,
    { cwd, timeout: 300000, encoding: 'utf-8', stdio: 'pipe' },
  );
  console.log(`Completion data generated for ${encounterType}.`);
}

/**
 * Aggregate per-encounter completion data into scope-level report_data
 * nodes (global + per health center), then clear Drupal caches.
 *
 * Note: execSync with hardcoded commands — same pattern as
 * recalculateLargeDatasets(). No user input involved.
 */
export function completionRecalculateLargeDatasets() {
  const { drushCmd, cwd } = drushEnv();
  console.log('Recalculating completion large datasets...');
  execSync(
    `${drushCmd} scr profiles/hedley/modules/custom/hedley_reports/scripts/completion-recalculate-large-datasets.php`,
    { cwd, timeout: 300000, encoding: 'utf-8', stdio: 'pipe' },
  );
  execSync(`${drushCmd} cc all`, {
    cwd, timeout: 30000, encoding: 'utf-8', stdio: 'pipe',
  });
  console.log('Completion large datasets recalculated.');
}

/**
 * Navigate to the Completion report results page for a Health Center scope.
 */
export async function navigateToCompletionReportPage(
  page: Page,
  healthCenterId: number,
) {
  const baseUrl = getDdevUrl();
  await page.goto(
    `${baseUrl}/admin/reports/completion/health-center/${healthCenterId}?t=${Date.now()}`,
  );
  await page.locator('.page-content.completion').waitFor({ timeout: 30000 });
}

/**
 * Select a report type from the Completion report dropdown.
 * Values: "acute-illness", "prenatal", "ncd", "hiv", "tuberculosis", etc.
 */
export async function selectCompletionReportType(page: Page, reportType: string) {
  const select = page
    .locator('.page-content.completion .select-input-wrapper')
    .first()
    .locator('select.select-input');
  await select.selectOption(reportType);
  await page.waitForTimeout(WAIT.sectionTransition);
}

/**
 * Select the "Taken By" filter on the Completion report.
 * Values: "nurse", "chw", or "" for Any (default).
 * Only visible for report types that support both roles.
 */
export async function selectCompletionTakenBy(page: Page, takenBy: string) {
  const select = page
    .locator('.page-content.completion .select-input-wrapper')
    .nth(1)
    .locator('select.select-input');
  await select.selectOption(takenBy);
  await page.waitForTimeout(WAIT.sectionTransition);
}

/**
 * Set the date range on the Completion report page.
 */
export async function setCompletionDateRange(
  page: Page,
  startDate: Date,
  limitDate: Date,
) {
  const dateInputs = page.locator('.page-content.completion div.form-input.date');
  await dateInputs.nth(0).click();
  await selectDateInCalendar(page, startDate);

  await page.waitForTimeout(WAIT.elmRerender);

  await dateInputs.nth(1).click();
  await selectDateInCalendar(page, limitDate);

  await page.waitForTimeout(WAIT.pageNavigation);
}

// ---------------------------------------------------------------------------
// Completion Report table reading
// ---------------------------------------------------------------------------

export interface CompletionTableRow {
  activity: string;
  expected: number;
  completed: number;
  percent: string;
}

export interface CompletionTableData {
  heading: string;
  rows: CompletionTableRow[];
}

/**
 * Read the Completion report table rendered by viewMetricsResultsTable.
 *
 * Structure:
 *   div.report.{reportClass}
 *     div.section.heading  → heading text
 *     div.table.wide
 *       div.row (captions — cells have class "item heading")
 *       div.row (data — cells have class "item value")
 *       ...
 *
 * @param reportClass - e.g., 'acute-illness', 'prenatal', 'ncd'
 */
export async function readCompletionTable(
  page: Page,
  reportClass: string,
): Promise<CompletionTableData> {
  const report = page.locator(`div.report.${reportClass}`);
  await report.waitFor({ timeout: 10000 });

  const heading = (await report.locator('div.section.heading').textContent())?.trim() ?? '';

  const table = report.locator('div.table.wide');
  const allRows = table.locator('div.row');
  const rowCount = await allRows.count();

  const rows: CompletionTableRow[] = [];
  // Skip first row (captions).
  for (let i = 1; i < rowCount; i++) {
    const cells = allRows.nth(i).locator('div.item');
    const cellCount = await cells.count();
    if (cellCount >= 4) {
      const activity = ((await cells.nth(0).textContent()) ?? '').replace(/\s+/g, ' ').trim();
      const expected = parseInt((await cells.nth(1).textContent())?.trim() ?? '0', 10) || 0;
      const completed = parseInt((await cells.nth(2).textContent())?.trim() ?? '0', 10) || 0;
      const percent = (await cells.nth(3).textContent())?.trim() ?? '0%';
      rows.push({ activity, expected, completed, percent });
    }
  }

  return { heading, rows };
}

/**
 * Find a row in the completion table by activity label.
 * Tries exact match first, then partial match.
 */
export function findCompletionRow(
  data: CompletionTableData,
  activityLabel: string,
): CompletionTableRow | undefined {
  return (
    data.rows.find(r => r.activity === activityLabel) ??
    data.rows.find(r => r.activity.includes(activityLabel))
  );
}
