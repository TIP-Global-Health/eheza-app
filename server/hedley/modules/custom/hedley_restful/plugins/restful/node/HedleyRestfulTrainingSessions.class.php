<?php

/**
 * @file
 * Contains \HedleyRestfulTrainingSessions.
 */

/**
 * Class HedleyRestfulTrainingSessions.
 */
class HedleyRestfulTrainingSessions extends HedleyRestfulSessions {

  /**
   * Overrides \RestfulDataProviderEFQ::controllersInfo().
   */
  public static function controllersInfo() {
    return [
      '' => [
        \RestfulInterface::POST => 'performAction',
      ],
    ];
  }

  /**
   * Handle the action for creating/deleting taining sessions.
   *
   * @return array
   *   The type of action carried out.
   *
   * @throws \RestfulBadRequestException
   */
  public function performAction() {
    $request = $this->getRequest();

    if (!isset($request['action']) || empty($request['action'])) {
      throw new RestfulBadRequestException('Action is not specified');
    }

    if ($request['action'] == 'create_all') {
      $result = $this->createTrainingEntities();
    }
    elseif ($request['action'] == 'delete_all') {
      $result = $this->deleteTrainingEntities();
    }
    else {
      $result = 'invalid';
    }

    // @todo: Fix this.
    return [
      0 => [
        'id' => 0,
        'action' => $result,
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
   * @return string|bool
   *   Whether training sessions were created or not and FALSE if user doesn't
   *   have access.
   */
  protected function createTrainingEntities() {
    if (!$this->checkTrainingSessionsAccess()) {
      // Check access, only admins should be able to preform this action.
      return FALSE;
    }

    // Get all clinics.
    $query = new EntityFieldQuery();
    $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'clinic')
      ->propertyCondition('status', NODE_PUBLISHED)
      ->propertyOrderBy('title', 'ASC');

    $clinic_nids = [];

    hedley_restful_query_in_batches($query, 50, function ($offset, $count, $batch_ids) use (&$clinic_nids) {
      $clinic_nids = array_merge($clinic_nids, $batch_ids);
    });
    $scheduled_date = date('Y-m-d', REQUEST_TIME);

    $sessions_created = FALSE;
    foreach ($clinic_nids as $clinic_nid) {
      if (hedley_schedule_clinic_has_sessions($clinic_nid, $scheduled_date)) {
        // Clinic has an active session for today, no need to create a session
        // for it.
        continue;
      }

      $request = [
        'clinic' => $clinic_nid,
        'training' => TRUE,
        'scheduled_date' => [
          'value' => $scheduled_date,
          'value2' => $scheduled_date,
        ],
      ];

      $this->setRequest($request);

      $this->createEntity();
      $sessions_created = TRUE;
    }

    return $sessions_created ? 'created' : 'no_creation';
  }

  /**
   * Delete all existing training sessions.
   *
   * This would be used by administrators on the sandbox website for deleting
   * all training sessions for meaning they will be all deleted when admin
   * clicks on the "Delete all training sessions" button, it will delete all
   * training session even if it's an old session opened on a past day.
   *
   * @return string|bool
   *   Training sessions were deleted or FALSE if user doesn't have access.
   *
   * @throws \EntityFieldQueryException
   */
  public function deleteTrainingEntities() {
    if (!$this->checkTrainingSessionsAccess()) {
      // Check access, only admins should be able to preform this action.
      return FALSE;
    }

    // Get all sessions.
    $query = new EntityFieldQuery();
    $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'session')
      ->propertyCondition('status', NODE_PUBLISHED)
      ->fieldCondition('field_training', 'value', TRUE)
      ->fieldOrderBy('field_scheduled_date', 'value', 'ASC');

    $session_nids = [];

    hedley_restful_query_in_batches($query, 50, function ($offset, $count, $batch_ids) use (&$session_nids) {
      $session_nids = array_merge($session_nids, $batch_ids);
    });

    // Delete all training sessions.
    foreach ($session_nids as $session_nid) {
      $this->deleteEntity($session_nid);
    }

    // Override Settings the HTTP headers because we want to return the action.
    $this->setHttpHeaders('Status', 200);

    return 'deleted';
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
