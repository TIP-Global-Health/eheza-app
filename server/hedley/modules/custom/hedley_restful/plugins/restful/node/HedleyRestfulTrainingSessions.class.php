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
   * @todo: Add docs.
   *
   * @return array
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

    return $result;
  }

  /**
   * Create training sessions for all existing clinics.
   *
   * This would be used by administrators on the sandbox website for creating
   * training sessions for all the existing clinics which are scheduled for the
   * day of creating them, meaning they will be open for the same day the admin
   * clicks on the "Create all training sessions" button.
   *
   * @throws \RestfulException
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
        ],
      ];

      $this->setRequest($request);

      try {
        $this->createEntity();
        $sessions_created = TRUE;
      }
      catch (Exception $e) {
        throw new RestfulException($e);
      }
    }

    return $sessions_created ? ['action' => 'created'] : ['action' => 'no_creation'];
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
