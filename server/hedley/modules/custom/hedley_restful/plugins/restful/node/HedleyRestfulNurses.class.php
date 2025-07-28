<?php

/**
 * @file
 * Contains \HedleyRestfulNurses.
 */

/**
 * Class HedleyRestfulNurses.
 */
class HedleyRestfulNurses extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $standard_fields_names = [
      'field_role',
      'field_pin_code',
      'field_resilience_program',
      'field_resilience_role',
      'field_education_level',
      'field_gender',
      'field_marital_status',
      'field_ubudehe',
      'field_next_reminder',
      'field_resilience_messages',
      'field_resilience_consent',
      'field_resilience_consent_reason',
    ];

    foreach ($standard_fields_names as $field_name) {
      $public_name = str_replace('field_', '', $field_name);
      $public_fields[$public_name] = [
        'property' => $field_name,
      ];
    }

    $public_fields['health_centers'] = [
      'property' => 'field_health_centers',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['villages'] = [
      'property' => 'field_villages',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['birth_date'] = [
      'property' => 'field_birth_date',
      'process_callbacks' => [
        [$this, 'convertTimestampToYmd'],
      ],
    ];

    $public_fields['resilience_start_date'] = [
      'property' => 'field_resilience_start_date',
      'process_callbacks' => [
        [$this, 'convertTimestampToYmd'],
      ],
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $field_names = [
      'field_role',
      'field_pin_code',
      'field_health_centers',
      'field_villages',
      'field_resilience_program',
      'field_resilience_role',
      'field_education_level',
      'field_gender',
      'field_marital_status',
      'field_ubudehe',
      'field_birth_date',
      'field_resilience_start_date',
      'field_next_reminder',
      'field_resilience_messages',
      'field_resilience_consent',
      'field_resilience_consent_reason',
    ];

    foreach ($field_names as $field_name) {
      hedley_general_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    $query->addExpression("GROUP_CONCAT(DISTINCT field_role.field_role_value)", 'field_role');

    // Get the UUIDs of the health centers.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_health_centers.field_health_centers_target_id", 'uuids_health_centers');
    $query->addExpression("GROUP_CONCAT(DISTINCT uuids_health_centers.field_uuid_value)", 'uuids_health_centers');

    // Get the UUIDs of the villages.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_villages.field_villages_target_id", 'uuids_villages');
    $query->addExpression("GROUP_CONCAT(DISTINCT uuids_villages.field_uuid_value)", 'uuids_villages');
    $query->groupBy('node.nid');
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->role = explode(',', $item->role);
      $item->health_centers = explode(',', $item->uuids_health_centers);
      unset($item->uuids_health_centers);

      if (empty($item->uuids_villages)) {
        $item->villages = [];
      }
      else {
        $item->villages = explode(',', $item->uuids_villages);
      }
      unset($item->uuids_villages);

      $item->resilience_program = (bool) $item->resilience_program;

      $birth_date = explode(' ', $item->birth_date);
      $item->birth_date = !empty($birth_date[0]) ? $birth_date[0] : NULL;

      $resilience_start_date = explode(' ', $item->resilience_start_date);
      $item->resilience_start_date = !empty($resilience_start_date[0]) ? $resilience_start_date[0] : NULL;
      $item->resilience_messages = drupal_json_decode($item->resilience_messages);
      $item->resilience_consent = (bool) $item->resilience_consent;
    }

    return $items;
  }

}
