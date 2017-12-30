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
    $account = $this->getAccount();

    $clinic_ids = hedley_restful_extract_ids(
      (new EntityFieldQuery())
        ->entityCondition('entity_type', 'node')
        ->entityCondition('bundle', 'clinic')
        ->propertyCondition('status', NODE_PUBLISHED)
        ->propertyOrderBy('title', 'ASC')
        ->range(0, 1000)
        ->execute()
    );

    return hedley_restful_output_from_handler('clinics', $clinic_ids, $account);
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
    $account = $this->getAccount();

    $session_ids = hedley_restful_extract_ids(
      (new EntityFieldQuery())
        ->entityCondition('entity_type', 'node')
        ->entityCondition('bundle', 'session')
        ->propertyCondition('status', NODE_PUBLISHED)
        ->fieldOrderBy('field_scheduled_date', 'value', 'ASC')
        ->range(0, 20000)
        ->execute()
    );

    return hedley_restful_output_from_handler('sessions', $session_ids, $account);
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
    $account = $this->getAccount();

    $clinic_id = entity_metadata_wrapper('node', $nid)->field_clinic->getIdentifier();

    // First, let us get all the mothers assigned to this clinic.
    $mother_ids = hedley_restful_extract_ids(
      (new EntityFieldQuery())
        ->entityCondition('entity_type', 'node')
        ->entityCondition('bundle', 'mother')
        ->fieldCondition('field_clinic', 'target_id', $clinic_id)
        ->propertyCondition('status', NODE_PUBLISHED)
        ->propertyOrderBy('title', 'ASC')
        ->range(0, 1000)
        ->execute()
    );

    // Then, get all the children of all the mothers. It's more
    // efficient to do this in one query than many.
    $child_ids = hedley_restful_extract_ids(
      (new EntityFieldQuery())
        ->entityCondition('entity_type', 'node')
        ->entityCondition('bundle', 'child')
        ->fieldCondition('field_mother', 'target_id', $mother_ids, "IN")
        ->propertyCondition('status', NODE_PUBLISHED)
        ->range(0, 2000)
        ->execute()
    );

    // Now, let's get all the child measurements.
    $child_bundles = [
      "height" => "heights",
      "muac" => "muacs",
      "nutrition" => "nutritions",
      "photo" => "photos",
      "weight" => "weights",
    ];

    // We order the measurements by date_measured descending, since it is
    // convenient for the client to have the most recent measurements first.
    $child_activity_ids = hedley_restful_extract_ids(
      (new EntityFieldQuery())
        ->entityCondition('entity_type', 'node')
        ->entityCondition('bundle', array_keys($child_bundles))
        ->fieldCondition('field_child', 'target_id', $child_ids, "IN")
        ->fieldOrderBy('field_date_measured', 'value', 'DESC')
        ->propertyCondition('status', NODE_PUBLISHED)
        ->range(0, 10000)
        ->execute()
    );

    $mother_bundles = [
      "family_planning" => "family-plannings",
    ];

    $mother_activity_ids = hedley_restful_extract_ids(
      (new EntityFieldQuery())
        ->entityCondition('entity_type', 'node')
        ->entityCondition('bundle', array_keys($mother_bundles))
        ->fieldCondition('field_mother', 'target_id', $mother_ids, "IN")
        ->fieldOrderBy('field_date_measured', 'value', 'DESC')
        ->propertyCondition('status', NODE_PUBLISHED)
        ->range(0, 10000)
        ->execute()
    );

    // Now, provide the usual output, since that's easiest. We'll
    // stitch together the structures we want on the client.
    $mother_output = hedley_restful_output_from_handler('mothers', $mother_ids, $account);
    $child_output = hedley_restful_output_from_handler('children', $child_ids, $account);

    $mother_activity_output = hedley_restful_output_for_bundles($mother_bundles, $mother_activity_ids, $account);
    $child_activity_output = hedley_restful_output_for_bundles($child_bundles, $child_activity_ids, $account);

    $grouped_mother_activity = [];
    $grouped_child_activity = [];

    // We group the mother mesaurements and child measurements by the mother or
    // child, and the type of the measurement, because it's really easy to do
    // that here, and it makes the decoder on the client side simpler.
    foreach ($mother_activity_output as $activity) {
      $grouped_mother_activity[$activity['mother']][$activity['type']][] = $activity;
    }

    foreach ($child_activity_output as $activity) {
      $grouped_child_activity[$activity['child']][$activity['type']][] = $activity;
    }

    return [
      "mothers" => $mother_output,
      "children" => $child_output,
      "mother_activity" => $grouped_mother_activity,
      "child_activity" => $grouped_child_activity,
    ];
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

    // Otherwise, check which clinics a user is assigned to
    // and compare that to the session's clinic.
    $session = entity_metadata_wrapper('node', $id);
    $wrapped_account = entity_metadata_wrapper('user', $account);

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

    // Now, let's get all the existing measurements for this session.
    $bundles = [
      "height" => "heights",
      "family_planning" => "family-plannings",
      "muac" => "muacs",
      "nutrition" => "nutritions",
      "photo" => "photos",
      "weight" => "weights",
    ];

    $activity_ids = hedley_restful_extract_ids(
      (new EntityFieldQuery())
        ->entityCondition('entity_type', 'node')
        ->entityCondition('bundle', array_keys($bundles))
        ->fieldCondition('field_session', 'target_id', $sessionId, "=")
        ->propertyCondition('status', NODE_PUBLISHED)
        ->range(0, 10000)
        ->execute()
    );

    $existing = [];
    node_load_multiple($activity_ids);

    foreach ($activity_ids as $id) {
      $wrapper = entity_metadata_wrapper('node', $id);
      if ($wrapper->__isset('field_child')) {
        $participant_id = $wrapper->field_child->getIdentifier();
      }
      else {
        $participant_id = $wrapper->field_mother->getIdentifier();
      }

      $existing[$participant_id][$wrapper->getBundle()] = $id;
    }

    $transaction = db_transaction();

    try {
      if ($request['closed']) {
        // We should close the session now.
        if (!($session->field_closed->value())) {
          $session->field_closed->set(TRUE);
          $session->save();
        }
      }

      foreach ($request['children'] as $childId => $edits) {
        foreach ($edits as $activity => $edit) {
          if ($bundles[$activity]) {
            $handler = restful_get_restful_handler($bundles[$activity]);
            $handler->setAccount($account);
            $previous = $existing[$childId][$activity];

            $this->handleEdit($handler, $edit, $previous);
          }
        }
      }

      foreach ($request['mothers'] as $motherId => $edits) {
        foreach ($edits as $activity => $edit) {
          if ($bundles[$activity]) {
            $handler = restful_get_restful_handler($bundles[$activity]);
            $handler->setAccount($account);
            $previous = $existing[$motherId][$activity];

            $this->handleEdit($handler, $edit, $previous);
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
      "id" => $sessionId,
    ];
  }

  /**
   * Execute a set of edits the client has made in a session.
   *
   * @param object $handler
   *   The Restful handler.
   * @param object $edit
   *   An describing the edit.
   * @param int $id
   *   The ID of the existing value for the session, if found.
   */
  public static function handleEdit($handler, $edit, $id) {
    // TODO: There is some obvious repeitition below.
    switch ($edit['tag']) {
      case 'created':
        // TODO: These should probably be a customization in the appropriate
        // handlers.
        $edit['value']['date_measured'] = strtotime($edit['value']['date_measured']);
        if ($edit['value']['photo']) {
          $edit['value']['photo'] = $edit['value']['photo']['id'];
        }

        if ($id) {
          // This is actually an update ... perhaps we ought to signal that
          // somehow?
          $handler->patch($id, $edit['value']);
        }
        else {
          $handler->post("", $edit['value']);
        }
        break;

      case 'edited':
        // TODO: These should probably be a customization in the appropriate
        // handlers.
        $edit['edited']['date_measured'] = strtotime($edit['edited']['date_measured']);
        if ($edit['edited']['photo']) {
          $edit['edited']['photo'] = $edit['edited']['photo']['id'];
        }

        if ($id) {
          $handler->patch($id, $edit['edited']);
        }
        else {
          // This is actually an update ... perhaps the value was deleted
          // behind our back?
          $handler->post("", $edit['edited']);
        }
        break;

      // TODO: Delete not implemented yet.
    }
  }

}
