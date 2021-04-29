<?php

/**
 * @file
 * Contains HedleyRestfulIndividualParticipants.
 */

/**
 * Class HedleyRestfulIndividualParticipants.
 */
class HedleyRestfulIndividualParticipants extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['person'] = [
      'property' => 'field_person',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['encounter_type'] = [
      'property' => 'field_encounter_type',
    ];

    $public_fields['expected'] = [
      'property' => 'field_expected',
      'process_callbacks' => [
        [$this, 'renderDate'],
      ],
    ];

    $public_fields['expected_date_concluded'] = [
      'property' => 'field_expected_date_concluded',
      'process_callbacks' => [
        [$this, 'renderDate2'],
      ],
    ];

    $public_fields['date_concluded'] = [
      'property' => 'field_date_concluded',
      'process_callbacks' => [
        [$this, 'renderDate2'],
      ],
    ];

    $public_fields['outcome'] = [
      'property' => 'field_outcome',
    ];

    $public_fields['outcome_location'] = [
      'property' => 'field_outcome_location',
    ];

    $public_fields['newborn'] = [
      'property' => 'field_newborn',
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
      'field_person',
      'field_encounter_type',
      'field_expected_date_concluded',
      'field_date_concluded',
      'field_outcome',
      'field_outcome_location',
      'field_deleted',
    ];

    foreach ($field_names as $field_name) {
      hedley_restful_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    hedley_restful_join_field_to_query($query, 'node', 'field_expected', FALSE, NULL, NULL, TRUE);

    // Get the UUIDs of the Person.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_person.field_person_target_id", 'uuid_person');
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->person = $item->uuid_person;
      unset($item->uuid_person);

      $value1 = $item->expected;
      $value2 = $item->field_expected_field_expected_value2;
      $item->expected = [
        'value' => $value1 ? hedley_restful_timestamp_only_date($value1) : NULL,
        'value2' => $value2 ? hedley_restful_timestamp_only_date($value2) : NULL,
      ];
      unset($item->field_expected_field_expected_value2);

      $date = explode(' ', $item->expected_date_concluded);
      $item->expected_date_concluded = !empty($date[0]) ? $date[0] : NULL;

      $date = explode(' ', $item->date_concluded);
      $item->date_concluded = !empty($date[0]) ? $date[0] : NULL;

      unset($item->label);
    }

    return $items;
  }

}
