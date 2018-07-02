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
   * @todo: Add docs.
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

    foreach ($all_clinics as $clinic) {
      if (hedley_schedule_clinic_has_sessions($clinic['id'], REQUEST_TIME)) {
        // Clinic has an active session for today, no need to create a session
        // for it.
        continue;
      }

      $request = [
        'clinic' => $clinic['id'],
        'training' => TRUE,
        'scheduled_date' => [
          'value' => REQUEST_TIME,
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

  protected function checkTrainingSessionsAccess() {
    return user_access('administer nodes');
  }
}
