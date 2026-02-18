<?php

/**
 * @file
 * Contains \HedleyRestfulFamilyEncounter.
 */

/**
 * Class HedleyRestfulFamilyEncounter.
 */
class HedleyRestfulFamilyEncounter extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['scheduled_date'] = [
      'property' => 'field_scheduled_date',
      'process_callbacks' => [
        [$this, 'renderDate'],
      ],
    ];

    $public_fields['health_center'] = [
      'property' => 'field_health_center',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['participants'] = [
      'property' => 'field_participants',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['deleted'] = [
      'property' => 'field_deleted',
    ];

    // The label is decorative only.
    unset($public_fields['label']);

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $field_names = [
      'field_health_center',
      'field_participants',
      'field_deleted',
    ];

    foreach ($field_names as $field_name) {
      hedley_general_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    hedley_general_join_field_to_query($query, 'node', 'field_scheduled_date', FALSE, NULL, NULL, TRUE);

    // Get the UUID of the Health Center.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_health_center.field_health_center_target_id", 'uuid_health_center');

    // Get the UUIDs of the Participants.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_participants.field_participants_target_id", 'uuid_participants');

    $query->addExpression("GROUP_CONCAT(DISTINCT uuid_participants.field_uuid_value)", 'participants_uuids');
    $query->groupBy('node.nid');
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->health_center = $item->uuid_health_center;
      unset($item->uuid_health_center);

      $item->participants = !empty($item->participants_uuids) ? explode(',', $item->participants_uuids) : [];
      unset($item->participants_uuids);

      $value1 = $item->scheduled_date;
      $value2 = $item->field_scheduled_date_field_scheduled_date_value2;
      $item->scheduled_date = [
        'value' => $value1 ? hedley_restful_timestamp_only_date($value1) : NULL,
        'value2' => $value2 ? hedley_restful_timestamp_only_date($value2) : NULL,
      ];
      unset($item->field_scheduled_date_field_scheduled_date_value2);

      unset($item->label);
    }

    return $items;
  }

}
