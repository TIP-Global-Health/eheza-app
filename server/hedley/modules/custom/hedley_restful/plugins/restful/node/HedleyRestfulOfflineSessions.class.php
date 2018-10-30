<?php

/**
 * @file
 * Contains \HedleyRestfulOfflineSessions.
 *
 * These are the same sessions as in the `sessions` endpoint, but specialized
 * to support going offline to take measurements and then upload those
 * measurements. So, everything is provided that is needed for that purpose,
 * unlike the `sessions` endpoint, which assumes that you're basically online.
 */

/**
 * Class HedleyRestfulOfflineSessions.
 */
class HedleyRestfulOfflineSessions extends HedleyRestfulEntityBaseNode {

  /**
   * Overrides \RestfulDataProviderEFQ::controllersInfo().
   */
  public static function controllersInfo() {
    // We only allow limited access to offline sessions ... you can download
    // one, and you can send a patch request with batched edits, which we will
    // handle specially.
    return [
      '^.*$' => [
        \RestfulInterface::GET => 'viewEntities',
        \RestfulInterface::HEAD => 'viewEntities',
        \RestfulInterface::PATCH => 'handleEdits',
      ],
    ];
  }

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['type'] = [
      'callback' => 'static::getType',
    ];

    $public_fields['closed'] = [
      'property' => 'field_closed',
    ];

    $public_fields['scheduled_date'] = [
      'property' => 'field_scheduled_date',
      'process_callbacks' => [
        [$this, 'renderDate'],
      ],
    ];

    $public_fields['clinic'] = [
      'property' => 'field_clinic',
      'resource' => [
        'clinic' => [
          'name' => 'clinics',
          'full_view' => FALSE,
        ],
      ],
    ];

    // We include basic data for all clinics, as this makes the offline UI
    // simpler. I suppose this could just be a second, independent HTTP
    // request, but including it here is simpler.
    $public_fields['clinics'] = [
      'property' => 'nid',
      'process_callbacks' => [
        [$this, 'getClinicData'],
      ],
    ];

    // We include basic data for all sessions for the relevant clinic, because
    // this is needed for the progress report.
    $public_fields['all_sessions'] = [
      'property' => 'field_clinic',
      'sub_property' => 'nid',
      'process_callbacks' => [
        [$this, 'getAllSessions'],
      ],
    ];

    $public_fields['participants'] = [
      'property' => 'nid',
      'process_callbacks' => [
        [$this, 'getParticipantData'],
      ],
    ];

    $public_fields['counseling_schedule'] = [
      'callback' => [$this, 'renderCounselingSchedule'],
    ];

    $public_fields['participant_forms'] = [
      'callback' => [$this, 'renderParticipantForms'],
    ];

    return $public_fields;
  }

  /**
   * Return the type of the entity.
   *
   * @return string
   *   The type name.
   */
  protected static function getType() {
    return 'session';
  }

  /**
   * Show the scheduled_date with date only.
   */
  public function renderDate($date) {
    return [
      'value' => $date['value'] ? hedley_restful_timestamp_only_date($date['value']) : NULL,
      'value2' => $date['value2'] ? hedley_restful_timestamp_only_date($date['value2']) : NULL,
    ];
  }

  /**
   * Return clinic data for all clinics.
   *
   * Of course, this could just be a separate, independent HTTP request, but
   * it's convenient to provide everything needed for an offline session at
   * once.
   *
   * @param int $nid
   *   The session node ID (not actually used, since we're getting all clinics).
   *
   * @return array
   *   Array with the RESTful output.
   */
  public function getClinicData($nid) {
    $query = new EntityFieldQuery();
    $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'clinic')
      ->propertyCondition('status', NODE_PUBLISHED)
      ->propertyOrderBy('title', 'ASC');

    $output = [];

    hedley_restful_query_in_batches($query, 50, function ($offset, $count, $clinic_ids) use (&$output) {
      $batch = hedley_restful_output_from_handler('clinics', $clinic_ids, $this->getAccount());
      $output = array_merge($output, $batch);
    });

    return $output;
  }

  /**
   * Return basic session data for all sessions for all clinics.
   *
   * Of course, this could just be a separate, independent HTTP request, but
   * it's convenient to provide everything needed for an offline session at
   * once.
   *
   * TODO: We could try to limit the number of sessions provided. Originally,
   * we only sent the sessions for the current clinic, but that doesn't work
   * quite right, because we could have measurements which were done for
   * other clinics ... for instance, in cases where a mother changed clinics.
   * So, for now, we'll just send all of them. It would be smarter to send just
   * some, but this will do for now.
   *
   * @param int $clinic_id
   *   The clinic ID for this session (not actually used, since now we're
   *   sending all the sessions).
   *
   * @return array
   *   Array with the RESTful output.
   */
  public function getAllSessions($clinic_id) {
    $query = new EntityFieldQuery();
    $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'session')
      ->propertyCondition('status', NODE_PUBLISHED)
      ->fieldOrderBy('field_scheduled_date', 'value', 'ASC');

    $output = [];

    hedley_restful_query_in_batches($query, 50, function ($offset, $count, $session_ids) use (&$output) {
      $batch = hedley_restful_output_from_handler('sessions', $session_ids, $this->getAccount());
      $output = array_merge($output, $batch);
    });

    return $output;
  }

  /**
   * Associate child measurement bundles and their handlers.
   *
   * @return array
   *   Array where they key is the bundle name and the value is the name of the
   *    handler.
   */
  public function getChildMeasurementBundles() {
    return [
      'height' => 'heights',
      'muac' => 'muacs',
      'nutrition' => 'nutritions',
      'photo' => 'photos',
      'weight' => 'weights',
      'counseling_session' => 'counseling-sessions',
    ];
  }

  /**
   * Associate mother measurement bundles and their handlers.
   *
   * @return array
   *   Array where they key is the bundle name and the value is the name of the
   *    handler.
   */
  public function getMotherMeasurementBundles() {
    // There will eventually be more of these.
    return [
      'family_planning' => 'family-plannings',
      'participant_consent' => 'participants-consent',
    ];
  }

  /**
   * Associate all measurement bundles and their handlers.
   *
   * @return array
   *   Array where they key is the bundle name and the value is the name of the
   *    handler.
   */
  public function getAllMeasurementBundles() {
    return $this->getChildMeasurementBundles() + $this->getMotherMeasurementBundles();
  }

  /**
   * Return participant data.
   *
   * @param int $nid
   *   The session node ID.
   *
   * @return array
   *   Array with the RESTful output.
   */
  public function getParticipantData($nid) {
    $clinic_id = entity_metadata_wrapper('node', $nid)->field_clinic->getIdentifier();

    $output = [
      'mothers' => [],
      'children' => [],
      'mother_activity' => [],
      'child_activity' => [],
    ];

    $mother_query = new EntityFieldQuery();
    $mother_query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'mother')
      ->fieldCondition('field_clinic', 'target_id', $clinic_id)
      ->propertyCondition('status', NODE_PUBLISHED)
      ->propertyOrderBy('title', 'ASC');

    hedley_restful_query_in_batches($mother_query, 50, function ($offset, $count, $mother_ids) use (&$output) {
      $mother_batch = hedley_restful_output_from_handler('mothers', $mother_ids, $this->getAccount());
      $output['mothers'] = array_merge($output['mothers'], $mother_batch);

      $child_query = new EntityFieldQuery();
      $child_query
        ->entityCondition('entity_type', 'node')
        ->entityCondition('bundle', 'child')
        ->fieldCondition('field_mother', 'target_id', $mother_ids, 'IN')
        ->propertyCondition('status', NODE_PUBLISHED);

      hedley_restful_query_in_batches($child_query, 50, function ($offset, $count, $child_ids) use (&$output) {
        $child_batch = hedley_restful_output_from_handler('children', $child_ids, $this->getAccount());
        $output['children'] = array_merge($output['children'], $child_batch);

        // We order the measurements by date_measured descending, since it is
        // convenient for the client to have the most recent measurements first.
        $child_activity_query = new EntityFieldQuery();
        $child_activity_query
          ->entityCondition('entity_type', 'node')
          ->entityCondition('bundle', array_keys($this->getChildMeasurementBundles()))
          ->fieldCondition('field_child', 'target_id', $child_ids, 'IN')
          ->fieldOrderBy('field_date_measured', 'value', 'DESC')
          ->propertyCondition('status', NODE_PUBLISHED);

        hedley_restful_query_in_batches($child_activity_query, 50, function ($offset, $count, $child_activity_ids) use (&$output) {
          $child_activity_batch = hedley_restful_output_for_bundles($this->getChildMeasurementBundles(), $child_activity_ids, $this->getAccount());

          // We group the mother mesaurements and child measurements by the
          // mother or child, and the type of the measurement, because it's
          // really easy to do that here, and it makes the decoder on the
          // client side simpler.
          foreach ($child_activity_batch as $activity) {
            // The `if` is silly, but otherwise coder thinks $activity is
            // unused, for some reason.
            if ($activity) {
              $output['child_activity'][$activity['child']][$activity['type']][] = $activity;
            }
          }
        });
      });

      $mother_activity_query = new EntityFieldQuery();
      $mother_activity_query
        ->entityCondition('entity_type', 'node')
        ->entityCondition('bundle', array_keys($this->getMotherMeasurementBundles()))
        ->fieldCondition('field_mother', 'target_id', $mother_ids, 'IN')
        ->fieldOrderBy('field_date_measured', 'value', 'DESC')
        ->propertyCondition('status', NODE_PUBLISHED);

      hedley_restful_query_in_batches($mother_activity_query, 50, function ($offset, $count, $mother_activity_ids) use (&$output) {
        $mother_activity_batch = hedley_restful_output_for_bundles($this->getMotherMeasurementBundles(), $mother_activity_ids, $this->getAccount());

        // Group the activities in a way that makes things easy for the client.
        foreach ($mother_activity_batch as $activity) {
          // The `if` is silly, but otherwise coder thinks $activity is unused,
          // for some reason.
          if ($activity) {
            $output['mother_activity'][$activity['mother']][$activity['type']][] = $activity;
          }
        }
      });
    });

    return $output;
  }

  /**
   * {@inheritdoc}
   *
   * Overridden so that we can provide some custom access control.
   */
  public function viewEntity($id) {
    $this->checkAccess($id);

    return parent::viewEntity($id);
  }

  /**
   * Check whether we should allow access to the specified session ID.
   *
   * Throws an exception if access should be denied.
   *
   * @param int $id
   *   The session ID.
   */
  public function checkAccess($id) {
    $account = $this->getAccount();
    $admin_role = user_role_load_by_name('administrator');

    // Let admins access any session.
    if (user_has_role($admin_role->rid, $account)) {
      return;
    }

    $session = entity_metadata_wrapper('node', $id);
    $wrapped_account = entity_metadata_wrapper('user', $account);

    // Check if the session is closed.
    if ($session->field_closed->value()) {
      throw new \RestfulForbiddenException('This session is closed.');
    }

    // Check if we're too far before the start date, or after the end.
    $start = $session->field_scheduled_date->value->value();
    $end = $session->field_scheduled_date->value2->value();

    // The rule is no access until one day before start. But, we also
    // have some timezone issues to worry about. So, in fact, we subtract
    // an extra day as an oversimplification.
    $no_access_before = $start - 86400 - 86400;

    // The rule is no access after the **end** of the day. But, we again
    // add an extra day to oversimplify timezone issues.
    $no_access_after = $end + 86400 + 86400;

    if (REQUEST_TIME < $no_access_before) {
      throw new \RestfulForbiddenException('This session is not available yet.');
    }

    if (REQUEST_TIME > $no_access_after) {
      throw new \RestfulForbiddenException('This session has passed its expiration date.');
    }

    // Check which clinics a user is assigned to
    // and compare that to the session's clinic.
    $target_clinic = $session->field_clinic->getIdentifier();
    $permitted_clinics = $wrapped_account->field_clinics->getIterator();

    // Iterate over the permitted clinics.
    foreach ($permitted_clinics as $clinic) {
      if ($clinic->getIdentifier() == $target_clinic) {
        return;
      }
    }

    // If we're still here, we don't have permission.
    throw new \RestfulForbiddenException('You do not have access to the clinic for this session.');
  }

  /**
   * Execute the edits the client has made in a session.
   *
   * The edits are passed in the JSON body of the request,
   * and take roughly the following form:
   *
   * - closed : Bool -- whether the session should be closed
   * - children : array of children
   * - mothers : array of mothers
   *
   * @param int $sessionId
   *   The session node ID.
   *
   * @return array
   *   Array with the RESTful output.
   */
  public function handleEdits($sessionId) {
    // Conceptually, we're "patching" the offline session with the edits made
    // on the client during the session. The JSON format we get here for the
    // edits is the same thing we cache in local storage on the client (for use
    // while we're offline). Then, we can send those edits up to this endpoint
    // as a PATCH request.
    //
    // We totally take over the PATCH verb ... if you want to patch the Session
    // entity in the normal way, use the Sessions endpoint instead.
    $request = $this->getRequest();
    $account = $this->getAccount();

    $this->checkAccess($sessionId);

    // Load the session.
    $session = entity_metadata_wrapper('node', $sessionId);

    // Get some metadata for our bundles.
    $bundles = $this->getAllMeasurementBundles();

    // We'd like this entire operation to succeed or fail as a whole, so that
    // we don't have deal with partially-successful updates. So, we create a
    // transaction.
    $transaction = db_transaction();

    try {
      if ($request['closed']) {
        // We should close the session now.
        if (!($session->field_closed->value())) {
          $session->field_closed->set(TRUE);
          $session->save();
        }
      }

      foreach ($request['children'] as $edits) {
        foreach ($edits as $activity => $edit) {
          if ($bundles[$activity]) {
            $handler = restful_get_restful_handler($bundles[$activity]);
            $handler->setAccount($account);

            $this->handleEdit($handler, $edit);
          }
          else {
            throw new RestfulBadRequestException("Entity $activity is unknown.");
          }
        }
      }

      foreach ($request['mothers'] as $edits) {
        foreach ($edits as $activity => $edit) {
          if ($bundles[$activity]) {
            $handler = restful_get_restful_handler($bundles[$activity]);
            $handler->setAccount($account);

            if (empty($edit['tag'])) {
              // For participant_consent, we can have multiple edits.
              foreach ($edit as $multiple) {
                $this->handleEdit($handler, $multiple);
              }
            }
            else {
              // For the others, it is just a single edit.
              $this->handleEdit($handler, $edit);
            }
          }
          else {
            // We can ignore the `checked_in` activity since we don't track it
            // on the backend. If sent another unrecognized activity, throw an
            // error.
            if ($activity != 'checked_in') {
              throw new RestfulBadRequestException("Entity $activity is unknown.");
            }
          }
        }
      }
    }
    catch (Exception $e) {
      $transaction->rollback();
      throw $e;
    }

    // We don't send back the whole offline session.
    return [
      'id' => $sessionId,
    ];
  }

  /**
   * Execute a set of edits the client has made in a session.
   *
   * @param object $handler
   *   The Restful handler.
   * @param object $edit
   *   An describing the edit.
   */
  public static function handleEdit($handler, $edit) {
    switch ($edit['tag']) {
      case 'created':
        $edit['value']['date_measured'] = strtotime($edit['value']['date_measured']);

        if ($edit['value']['photo']) {
          $edit['value']['photo'] = $edit['value']['photo']['id'];
        }

        $handler->post('', $edit['value']);
        break;

      case 'edited':
        $edit['edited']['date_measured'] = strtotime($edit['edited']['date_measured']);

        if ($edit['edited']['photo']) {
          $edit['edited']['photo'] = $edit['edited']['photo']['id'];
        }

        $handler->patch($edit['id'], $edit['edited']);

    }
  }

}
