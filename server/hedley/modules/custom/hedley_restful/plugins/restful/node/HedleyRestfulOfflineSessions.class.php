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

    $child_output = [];
    node_load_multiple($child_ids);

    foreach ($child_ids as $nid) {
      $handler = restful_get_restful_handler('children');
      $handler->setAccount($this->getAccount());
      $response = $handler->get($nid);

      $child_output[] = $response[0];
    }

    return [
      "mothers" => $mother_output,
      "children" => $child_output,
    ];
  }

}
