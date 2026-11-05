import { test, expect, Page } from '@playwright/test';
import { click, setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import { WAIT, formInput, selectByLabel } from './helpers/common';
import {
  createMotherOnAttendancePage,
  navigateToNurseGroupSession,
} from './helpers/group-session';

// The birth-date picker on the registration form, read from the screen.
//
// A child-only picker must start on "13 years ago, plus one day": a child born
// exactly 13 years ago counts as an adult on save, so offering that date lets
// the nurse pick a birth date the form then refuses. Pickers that accept any
// age, and the graduating-age limit of a group's child picker, stay as they
// are.
//
// Test titles avoid the program names CI uses to split the e2e jobs, so this
// spec runs in the general job.

type CalendarDate = { year: number; month: number; day: number };

function daysInMonth(year: number, month: number): number {
  return new Date(Date.UTC(year, month, 0)).getUTCDate();
}

/**
 * Move a calendar date by whole months, keeping the day where the target
 * month has it and using the month's last day otherwise (2024-02-29 minus
 * 13 years is 2011-02-28), the way the app's date library does.
 */
function addMonths(date: CalendarDate, months: number): CalendarDate {
  const index = date.year * 12 + (date.month - 1) + months;
  const year = Math.floor(index / 12);
  const month = (index % 12) + 1;
  return { year, month, day: Math.min(date.day, daysInMonth(year, month)) };
}

function addDays(date: CalendarDate, days: number): CalendarDate {
  const moved = new Date(Date.UTC(date.year, date.month - 1, date.day + days));
  return {
    year: moved.getUTCFullYear(),
    month: moved.getUTCMonth() + 1,
    day: moved.getUTCDate(),
  };
}

function iso(date: CalendarDate): string {
  const pad = (n: number) => n.toString().padStart(2, '0');
  return `${date.year}-${pad(date.month)}-${pad(date.day)}`;
}

/**
 * Today as the app sees it: the browser's local calendar day.
 */
async function today(page: Page): Promise<CalendarDate> {
  return page.evaluate(() => {
    const now = new Date();
    return {
      year: now.getFullYear(),
      month: now.getMonth() + 1,
      day: now.getDate(),
    };
  });
}

/**
 * Open the birth-date calendar of a form whose birth date is still empty and
 * choose the earliest year it lists. The calendar keeps every choice within
 * its range, so with no date chosen yet this lands on the earliest birth date
 * the picker offers.
 *
 * Returns that date, the years the picker lists, and the class of each of the
 * 42 day cells of the month shown (the grid starts on the Monday on or before
 * the 1st). The popup is left open.
 */
async function chooseEarliestBirthDate(page: Page) {
  await click(page.locator('.date-input').first(), page);
  const popup = page.locator('.ui.active.modal.calendar-popup');
  await popup.waitFor({ timeout: 5000 });

  const yearSelect = popup.locator('div.calendar > div.year > select');
  const years = (await yearSelect.locator('option').evaluateAll((options) =>
    options.map((option) => (option as HTMLOptionElement).value),
  ))
    .filter((value) => value !== '')
    .map((value) => parseInt(value, 10));
  const minYear = Math.min(...years);
  const maxYear = Math.max(...years);

  await yearSelect.selectOption(minYear.toString());
  await page.waitForTimeout(WAIT.elmRerender);

  const year = parseInt(await yearSelect.inputValue(), 10);
  const month = parseInt(
    await popup.locator('div.calendar > div.month > select').inputValue(),
    10,
  );
  const day = parseInt(
    (await popup
      .locator('div.calendar table tbody td.date-selector--selected')
      .innerText()).trim(),
    10,
  );
  const cellClasses = await popup
    .locator('div.calendar table tbody td')
    .evaluateAll((cells) => cells.map((cell) => cell.className));

  return { earliest: { year, month, day }, minYear, maxYear, cellClasses };
}

/**
 * The class of the day cell showing `date` in the grid of the given month, or
 * null when the grid does not show that date.
 */
function cellClassOf(
  cellClasses: string[],
  shownMonth: CalendarDate,
  date: CalendarDate,
): string | null {
  const first = Date.UTC(shownMonth.year, shownMonth.month - 1, 1);
  const mondayOffset = (new Date(first).getUTCDay() + 6) % 7;
  const gridStart = first - mondayOffset * 86400000;
  const index = Math.round(
    (Date.UTC(date.year, date.month - 1, date.day) - gridStart) / 86400000,
  );
  return index >= 0 && index < cellClasses.length ? cellClasses[index] : null;
}

async function confirmCalendar(page: Page) {
  await click(page.locator('.ui.active.modal.calendar-popup div.ui.button'), page);
  await page
    .locator('.ui.active.modal.calendar-popup')
    .waitFor({ state: 'hidden', timeout: 3000 })
    .catch(() => {});
}

/**
 * Dashboard → Clinical → Individual Assessment → the given encounter type →
 * "Register a new participant", up to the empty registration form.
 */
async function openIndividualRegistration(page: Page, encounterTypeText: string) {
  await click(page.locator('.icon-task-clinical'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });
  await click(page.locator('button.individual-assessment'), page);
  await page.locator('div.page-encounter-types').waitFor({ timeout: 10000 });
  await click(
    page.locator('button.encounter-type', { hasText: encounterTypeText }),
    page,
  );
  await page.locator('div.page-participants').waitFor({ timeout: 10000 });
  await click(
    page.locator('button.ui.primary.button.fluid', {
      hasText: 'Register a new participant',
    }),
    page,
  );
  await page
    .locator('.ui.grid .column', { hasText: 'First Name:' })
    .waitFor({ timeout: 10000 });
}

test.describe('Registration: child birth-date range', () => {
  test.describe.configure({ timeout: 300000 });

  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  test.describe('nurse', () => {
    test.beforeEach(async ({ page }) => {
      resetDevice();
      await setupDevice(page, '1234', 'Nyange Health Center');
    });

    test('Standard Pediatric Visit: the earliest birth date offered is 13 years ago plus one day, and it saves as a child', async ({ page }) => {
      // Scenario: a nurse registers a new child for a Standard Pediatric Visit
      // and picks the earliest birth date the calendar offers.
      // Criteria: the earliest date is 13 years ago plus one day; exactly 13
      // years ago is not offered; the earliest date saves as a child, without
      // asking for education level or marital status.
      await openIndividualRegistration(page, 'Standard Pediatric Visit');
      const now = await today(page);
      const thirteenYearsAgo = addMonths(now, -13 * 12);
      const expectedEarliest = addDays(thirteenYearsAgo, 1);

      const { earliest, maxYear, cellClasses } = await chooseEarliestBirthDate(page);
      expect(iso(earliest), 'earliest birth date offered').toBe(iso(expectedEarliest));
      expect(maxYear, 'latest year offered').toBe(now.year);

      const exactClass = cellClassOf(cellClasses, earliest, thirteenYearsAgo);
      if (exactClass !== null) {
        expect(exactClass, `${iso(thirteenYearsAgo)} must not be selectable`).toContain(
          'date-selector--disabled',
        );
      }

      await confirmCalendar(page);
      await page.waitForTimeout(WAIT.sectionTransition);

      await formInput(page, 'First Name:').fill(`TestChild${Date.now()}`);
      await formInput(page, 'Second Name:').fill('E2ETest');
      await page
        .locator('.ui.grid')
        .filter({ hasText: 'Gender:' })
        .locator('input[type="radio"]')
        .first()
        .check();

      // A child's form: mode of delivery is asked, education level and
      // marital status are not.
      await expect(
        page.locator('.ui.grid').filter({ hasText: 'Level of Education:' }),
      ).toHaveCount(0);
      await expect(
        page.locator('.ui.grid').filter({ hasText: 'Marital Status:' }),
      ).toHaveCount(0);
      await selectByLabel(page, 'Mode of delivery:', 1);

      await selectByLabel(page, 'Province:', 1);
      await page.waitForTimeout(WAIT.elmRerender);
      await selectByLabel(page, 'District:', 1);
      await page.waitForTimeout(WAIT.elmRerender);
      await selectByLabel(page, 'Sector:', 1);
      await page.waitForTimeout(WAIT.elmRerender);
      await selectByLabel(page, 'Cell:', 1);
      await page.waitForTimeout(WAIT.elmRerender);
      await selectByLabel(page, 'Village:', 1);
      await page
        .locator('.ui.grid')
        .filter({ hasText: 'Health Center:' })
        .locator('select')
        .selectOption({ label: 'Nyange Health Center' });

      // Save goes through to the participant page.
      await click(page.locator('button[type="submit"]'), page);
      await page
        .locator('div.page-participant.individual.well-child')
        .waitFor({ timeout: 30000 });
    });

    test('FBF group: a child added to a mother starts at the graduating age, 26 months back', async ({ page }) => {
      // Scenario: in an FBF group (neither Sorwathe nor ACHI), a nurse adds a
      // child to a newly registered mother.
      // Criterion: the graduating-age limit for other group types is
      // unchanged: the earliest birth date offered is 26 months ago.
      await navigateToNurseGroupSession(page, 'FBF', 'Nyange I');
      await createMotherOnAttendancePage(page);

      await click(page.locator('.add-participant-icon'), page);
      await page.locator('div.page-people').waitFor({ timeout: 10000 });
      await click(
        page.locator('button.ui.primary.button.fluid', {
          hasText: 'Register a new participant',
        }),
        page,
      );
      await page
        .locator('.ui.grid .column', { hasText: 'First Name:' })
        .waitFor({ timeout: 10000 });

      const now = await today(page);
      const { earliest } = await chooseEarliestBirthDate(page);
      expect(iso(earliest), 'earliest birth date offered').toBe(iso(addMonths(now, -26)));
    });
  });

  test.describe('CHW', () => {
    test.beforeEach(async ({ page }) => {
      resetDevice();
      await setupDevice(page, '2345', 'Akanduga');
    });

    test('TB Management: the picker still accepts any age, 120 years back to today', async ({ page }) => {
      // Scenario: a CHW registers a new participant for TB Management, whose
      // picker accepts any age.
      // Criterion: pickers that accept any age are unchanged: the earliest
      // birth date offered is 120 years ago, the latest is today.
      await openIndividualRegistration(page, 'TB Management');
      const now = await today(page);

      const { earliest, minYear, maxYear } = await chooseEarliestBirthDate(page);
      expect(minYear, 'earliest year offered').toBe(now.year - 120);
      expect(maxYear, 'latest year offered').toBe(now.year);
      expect(iso(earliest), 'earliest birth date offered').toBe(iso(addMonths(now, -120 * 12)));
    });
  });
});
