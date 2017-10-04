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
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['type'] = [
      'callback' => 'static::getType',
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
          'full_view' => TRUE,
        ],
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
   * Return participant data.
   *
   * @param int $nid
   *   The session node ID.
   *
   * @return array
   *   Array with the RESTful output.
   */
  public function getParticipantData($nid) {
    $wrapper = entity_metadata_wrapper('node', $nid);

    // First, let us get all the mothers assigned to this clinic.
    $mothers = (new EntityFieldQuery())
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'mother')
      ->fieldCondition('field_clinic', 'target_id', $wrapper->field_clinic->getIdentifier())
      ->propertyCondition('status', NODE_PUBLISHED)
      ->range(0, 1000)
      ->execute();

    $mother_ids = empty($mothers['node']) ? [] : array_keys($mothers['node']);

    // Then, get all the children of all the mothers. It's more
    // efficient to do this in one query than many.
    $children = (new EntityFieldQuery())
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'child')
      ->fieldCondition('field_mother', 'target_id', $mother_ids, "IN")
      ->propertyCondition('status', NODE_PUBLISHED)
      ->range(0, 2000)
      ->execute();

    $child_ids = empty($children['node']) ? [] : array_keys($children['node']);

    // Now, let's get all the child measurements.
    $child_bundles = [
      "height" => "heights",
      "muac" => "muacs",
      "nutrition" => "nutritions",
      "photo" => "photos",
      "weight" => "weights",
    ];

    $child_activities = (new EntityFieldQuery())
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', array_keys($child_bundles))
      ->fieldCondition('field_child', 'target_id', $child_ids, "IN")
      ->propertyCondition('status', NODE_PUBLISHED)
      ->range(0, 10000)
      ->execute();

    $child_activity_ids = empty($child_activities['node']) ? [] : array_keys($child_activities['node']);

    $mother_bundles = [
      "family-planning" => "family-plannings",
    ];

    $mother_activities = (new EntityFieldQuery())
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', array_keys($mother_bundles))
      ->fieldCondition('field_mother', 'target_id', $mother_ids, "IN")
      ->propertyCondition('status', NODE_PUBLISHED)
      ->range(0, 10000)
      ->execute();

    $mother_activity_ids = empty($mother_activities['node']) ? [] : array_keys($mother_activities['node']);

    // Now, provide the usual output, since that's easiest. We'll
    // stitch together the structures we want on the client.
    $mother_output = [];
    node_load_multiple($mother_ids);

    foreach ($mother_ids as $nid) {
      $handler = restful_get_restful_handler('mothers');
      $handler->setAccount($this->getAccount());
      $response = $handler->get($nid);

      $mother_output[] = $response[0];
    }

    $mother_activity_output = [];
    node_load_multiple($mother_activity_ids);

    foreach ($mother_activity_ids as $nid) {
      $wrapper = entity_metadata_wrapper('node', $nid);

      $handler = restful_get_restful_handler($mother_bundles[$wrapper->getBundle()]);
      $handler->setAccount($this->getAccount());
      $response = $handler->get($nid);

      $mother_activity_output[] = $response[0];
    }

    $child_output = [];
    node_load_multiple($child_ids);

    foreach ($child_ids as $nid) {
      $handler = restful_get_restful_handler('children');
      $handler->setAccount($this->getAccount());
      $response = $handler->get($nid);

      $child_output[] = $response[0];
    }

    $child_activity_output = [];
    node_load_multiple($child_activity_ids);

    foreach ($child_activity_ids as $nid) {
      $wrapper = entity_metadata_wrapper('node', $nid);

      $handler = restful_get_restful_handler($child_bundles[$wrapper->getBundle()]);
      $handler->setAccount($this->getAccount());
      $response = $handler->get($nid);

      $child_activity_output[] = $response[0];
    }

    return [
      "mothers" => $mother_output,
      "children" => $child_output,
      "mother_activity" => $mother_activity_output,
      "child_activity" => $child_activity_output,
    ];
  }

}
