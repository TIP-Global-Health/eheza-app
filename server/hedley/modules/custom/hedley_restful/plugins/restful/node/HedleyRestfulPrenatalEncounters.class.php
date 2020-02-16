<?php

/**
 * @file
 * Contains \HedleyRestfulPrenatalEncounters.
 */

/**
 * Class HedleyRestfulPrenatalEncounters.
 */
class HedleyRestfulPrenatalEncounters extends HedleyRestfulSyncBase {

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

    $public_fields['individual_participant'] = [
      'property' => 'field_individual_participant',
      'sub_property' => 'field_uuid',
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
    $field_names = [
      'field_individual_participant',
    ];

    foreach ($field_names as $field_name) {
      hedley_restful_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    hedley_restful_join_field_to_query($query, 'node', 'field_scheduled_date', FALSE, NULL, NULL, TRUE);

    // Get the UUIDs of the Individyul participant.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_individual_participant.field_individual_participant_target_id", 'uuid_individual_participant');
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->individual_participant = $item->uuid_individual_participant;
      unset($item->uuid_individual_participant);

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
