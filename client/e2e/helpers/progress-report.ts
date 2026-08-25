import { Page } from '@playwright/test';
import { click } from './auth';
import { WAIT } from './common';

/**
 * Opening a progress report differs per module in three ways: which tab
 * leads to it, whether that tab opens the report directly or first reveals
 * a list of report links, and what the report page itself looks like.
 * Everything a spec needs to know about that lives in the table below.
 */
export type ReportModule =
  | 'acute-illness'
  | 'child-scoreboard'
  | 'family-nutrition'
  | 'ncd'
  | 'nutrition'
  | 'prenatal'
  | 'prenatal-demographics'
  | 'tuberculosis'
  | 'well-child';

interface ReportConfig {
  /** Tab on the encounter page that leads to the report. */
  tab: string;
  /**
   * Label of the report link the tab reveals. Absent for modules where the
   * tab opens the report itself.
   */
  link?: string;
  /** Root of the report page. */
  root: string;
  /** Root of the encounter page the report was opened from. */
  encounter: string;
}

const REPORTS: Record<ReportModule, ReportConfig> = {
  'acute-illness': {
    tab: '#reports-tab',
    root: 'div.page-report.acute-illness',
    encounter: 'div.page-encounter.acute-illness',
  },
  'child-scoreboard': {
    tab: '#scorecard-tab',
    root: 'div.page-report.well-child',
    encounter: 'div.page-encounter.child-scoreboard',
  },
  'family-nutrition': {
    tab: '#reports-tab',
    root: 'div.page-report.family-nutrition',
    encounter: 'div.page-encounter.family-nutrition',
  },
  ncd: {
    tab: '#reports-tab',
    link: 'Progress Report',
    root: 'div.page-report.ncd',
    encounter: 'div.page-encounter.ncd',
  },
  nutrition: {
    tab: '#reports-tab',
    link: 'Progress Report',
    root: 'div.page-report.well-child',
    encounter: 'div.page-encounter.nutrition',
  },
  prenatal: {
    tab: '#reports-tab',
    link: 'Clinical Progress Report',
    root: 'div.page-report.clinical',
    encounter: 'div.page-encounter.prenatal',
  },
  'prenatal-demographics': {
    tab: '#reports-tab',
    link: 'Demographics Report',
    root: 'div.page-report.demographics',
    encounter: 'div.page-encounter.prenatal',
  },
  tuberculosis: {
    tab: '#reports-tab',
    link: 'Progress Report',
    root: 'div.page-report.tuberculosis',
    encounter: 'div.page-encounter.tuberculosis',
  },
  'well-child': {
    tab: '#reports-tab',
    root: 'div.page-report.well-child',
    encounter: 'div.page-encounter.well-child',
  },
};

/**
 * Open the progress report from the encounter page, and return its root
 * locator so the caller can assert against what it holds.
 */
export async function openReport(page: Page, module: ReportModule) {
  const config = REPORTS[module];
  const report = page.locator(config.root);

  // Saving the last activity of an encounter opens the report by itself in
  // some modules, so the tab is only needed when it is not already open.
  const alreadyOpen = await report.isVisible({ timeout: 1000 }).catch(() => false);
  if (alreadyOpen) {
    return report;
  }

  const tab = page.locator(config.tab);
  await tab.waitFor({ timeout: 15000 });
  await click(tab, page);
  await page.waitForTimeout(WAIT.elmRerender);

  if (config.link) {
    const reportLink = page
      .locator('.reports-wrapper .report-wrapper')
      .filter({ hasText: config.link });
    await reportLink.waitFor({ timeout: 10000 });
    await click(reportLink, page);
  }

  await report.waitFor({ timeout: 15000 });
  await page.waitForTimeout(WAIT.elmRerender);

  return report;
}

/**
 * Return from the progress report to the encounter it was opened from.
 * The back arrow carries the click handler on the link in some modules and
 * on the icon inside it in others, so click the icon and let it bubble.
 */
export async function closeReport(page: Page, module: ReportModule) {
  const config = REPORTS[module];

  await click(page.locator(`${config.root} .link-back .icon-back`), page);
  await page.locator(config.encounter).waitFor({ timeout: 15000 });
  await page.waitForTimeout(WAIT.elmRerender);
}
