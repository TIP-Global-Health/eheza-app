import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import { syncAndWait } from './helpers/common';
import {
  navigateToEducationSession,
  selectTopics,
  toggleAllParticipants,
  selectParticipant,
  endEducationSession,
  queryEducationSessionNodes,
} from './helpers/education-session';

test.describe('CHW: Group Education Session', () => {
  test.describe.configure({ timeout: 600000 });

  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  // Login as CHW Jojo (PIN 2345), select Akanduga village.
  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page, '2345', 'Akanduga');
  });

  test('Education session with multiple topics and participants', async ({
    page,
  }) => {
    // Scenario: CHW creates a group education session, selects 3 topics
    //   (Tuberculosis, Malaria, Family Planning), marks 2 participants
    //   from demo data, and records the session.
    // Flow: GroupEncounterTypesPage → Topics → Attendance → Record → Sync.
    // Backend: Verifies education_session node exists with correct topics,
    //   participants, and end date set.

    // 1. Navigate to GroupEncounterTypesPage → click "Health Education".
    await navigateToEducationSession(page);

    // 2. Select 3 topics.
    await selectTopics(page, [
      'Tuberculosis',
      'Malarial diseases',
      'Family Planning',
    ]);

    // 3. Display all participants and select 2.
    await toggleAllParticipants(page);
    const participant1 = await selectParticipant(page, 0);
    const participant2 = await selectParticipant(page, 1);

    console.log(`Selected participants: "${participant1}", "${participant2}"`);

    // 4. Record the session (navigates back to Dashboard).
    await endEducationSession(page);

    // 5. Sync and verify.
    await syncAndWait(page, 'Nyange Health Center');

    // 6. Query backend for the education session.
    // Participant names in the UI are display names (e.g., "Jane Doe"),
    // which match Drupal title format ("Doe Jane") since the village list
    // shows names from the person node title.
    const result = queryEducationSessionNodes(participant1, [
      'tuberculosis',
      'malaria',
      'family-planning',
    ]);

    expect(result.found).toBe(true);
    expect(result.participantFound).toBe(true);
    expect(result.hasEndDate).toBe(true);
    expect(result.allTopicsFound).toBe(true);
  });
});
