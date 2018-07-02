<?php

/**
 * @file
 * Contains \HedleyRestfulTrainingSessions.
 */

/**
 * Class HedleyRestfulTrainingSessions.
 */
class HedleyRestfulTrainingSessions extends HedleyRestfulOfflineSessions {

  /**
   * Overrides \RestfulDataProviderEFQ::controllersInfo().
   */
  public static function controllersInfo() {
    return [
      '' => [
        \RestfulInterface::POST => 'createTrainingEntities',
      ],
      '^.*$' => [
        \RestfulInterface::DELETE => 'deleteTrainingEntities',
      ],
    ];
  }

  /**
   * Create training sessions for all existing clinics.
   *
   * This would be used by administrators on the sandbox website for creating
   * training sessions for all the existing clinics which are scheduled for the
   * day of creating them, meaning they will be open for the same day the admin
   * clicks on the "Create all training sessions" button.
   *
   * @throws \RestfulForbiddenException
   */
  public function createTrainingEntities() {
    if (!$this->checkTrainingSessionsAccess()) {
      // Check access, only admins should be able to preform this action.
      return;
    }

    // Get all clinics.
    $all_clinics = $this->getClinicData();
    $scheduled_date = date('Y-m-d', REQUEST_TIME);

    foreach ($all_clinics as $clinic) {
      if (hedley_schedule_clinic_has_sessions($clinic['id'], $scheduled_date)) {
        // Clinic has an active session for today, no need to create a session
        // for it.
        continue;
      }

      $request = [
        'clinic' => $clinic['id'],
        'training' => TRUE,
        'scheduled_date' => [
          'value' => $scheduled_date,
        ],
      ];

      $this->setRequest($request);

      try {
        $this->createEntity();
      }
      catch (Exception $e) {
        throw new RestfulForbiddenException($e);
      }
    }
  }

  /**
   * Check if the current user has permissions to administer nodes.
   *
   * @return bool
   *   Boolean TRUE if the user has the requested permission.
   */
  protected function checkTrainingSessionsAccess() {
    return user_access('administer nodes', $this->getAccount());
  }
}
