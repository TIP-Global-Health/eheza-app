<?php

/**
 * @file
 * Contains \HedleyRestfulSessions.
 */

/**
 * Class HedleyRestfulSessions.
 */
class HedleyRestfulSessions extends HedleyRestfulSyncBase {

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

    $public_fields['clinic'] = [
      'property' => 'field_clinic',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['clinic_type'] = [
      'property' => 'field_clinic',
      'sub_property' => 'field_group_type',
    ];

    // The label is decorative only.
    unset($public_fields['label']);

    return $public_fields;
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
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    hedley_restful_join_field_to_query($query, 'node', 'field_clinic', FALSE);
    hedley_restful_join_field_to_query($query, 'node', 'field_scheduled_date', FALSE, NULL, NULL, TRUE);
    // Get the UUID of the Clinic.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_clinic.field_clinic_target_id", 'uuid_clinic');
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->clinic = $item->uuid_clinic;
      unset($item->uuid_clinic);

      $value1 = $item->scheduled_date;
      $value2 = $item->field_scheduled_date_field_scheduled_date_value2;
      $item->scheduled_date = [
        'value' => $value1 ? hedley_restful_timestamp_only_date($value1) : NULL,
        'value2' => $value2 ? hedley_restful_timestamp_only_date($value2) : NULL,

      ];
      unset($item->field_scheduled_date_field_scheduled_date_value2);
    }

    return $items;
  }

}
