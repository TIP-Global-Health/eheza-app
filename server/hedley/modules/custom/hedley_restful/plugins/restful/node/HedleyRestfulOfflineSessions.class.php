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
      "family-planning" => "family-plannings",
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
    //
    // Here's an example of what the JSON would look like. We could, of course,
    // massage this on the client to send something different, if that ends up
    // being convenient here.
    /*
    {
    "children": {
    "48": {
    "height": {
    "tag": "created",
    "value": {
    "child": 48,
    "date_measured": "2017-11-24",
    "height": 23,
    "session": 226
    }
    },
    "muac": {
    "tag": "unedited"
    },
    "nutrition": {
    "tag": "unedited"
    },
    "photo": {
    "tag": "unedited"
    },
    "weight": {
    "tag": "created",
    "value": {
    "child": 48,
    "date_measured": "2017-11-24",
    "session": 226,
    "weight": 23
    }
    }
    }
    },
    "closed": true,
    "mothers": {
    "26": {
    "checked-in": true,
    "family-planning": {
    "tag": "unedited"
    }
    },
    "29": {
    "checked-in": true,
    "family-planning": {
    "tag": "unedited"
    }
    },
    "33": {
    "checked-in": true,
    "family-planning": {
    "tag": "unedited"
    }
    },
    "41": {
    "checked-in": true,
    "family-planning": {
    "tag": "created",
    "value": {
    "date_measured": "2017-11-24",
    "family_planning_signs": [
    "condoms",
    "pill"
    ],
    "mother": 41,
    "session": 226
    }
    }
    }
    }
    }
     */

    $request = $this->getRequest();

    // Load the session.
    $session = entity_metadata_wrapper('node', $sessionId);

    $transaction = db_transaction();

    try {
      if ($request['closed']) {
        // We should close the session now.
        if (!($session->field_closed->value())) {
          $session->field_closed->set(TRUE);
          $session->save();
        }
      }

      // TODO: Implement the edits and creates
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

}
