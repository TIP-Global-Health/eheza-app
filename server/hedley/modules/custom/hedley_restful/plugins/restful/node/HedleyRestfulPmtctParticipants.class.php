<?php

/**
 * @file
 * Contains HedleyRestfulPmtctParticipants.
 */

/**
 * Class HedleyRestfulPmtctParticipants.
 */
class HedleyRestfulPmtctParticipants extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $standard_fields_names = [
      'field_adult_activities',
    ];

    foreach ($standard_fields_names as $field_name) {
      $public_name = str_replace('field_', '', $field_name);
      $public_fields[$public_name] = [
        'property' => $field_name,
      ];
    }

    $public_fields['person'] = [
      'property' => 'field_person',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['adult'] = [
      'property' => 'field_adult',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['clinic'] = [
      'property' => 'field_clinic',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['expected'] = [
      'property' => 'field_expected',
      'process_callbacks' => [
        [$this, 'renderDate'],
      ],
    ];

    // The label is decorative only.
    unset($public_fields['label']);

    return $public_fields;
  }

  /**
   * Show the date only.
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
      'field_adult_activities',
      'field_person',
      'field_adult',
      'field_clinic',
    ];

    foreach ($field_names as $field_name) {
      hedley_restful_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    hedley_restful_join_field_to_query($query, 'node', 'field_expected', FALSE, NULL, NULL, TRUE);

    // Get the UUIDs of the Person, Adult and Clinic.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_person.field_person_target_id", 'uuid_person');
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_adult.field_adult_target_id", 'uuid_adult');
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_clinic.field_clinic_target_id", 'uuid_clinic');
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->person = $item->uuid_person;
      unset($item->uuid_person);
      $item->adult = $item->uuid_adult;
      unset($item->uuid_adult);
      $item->clinic = $item->uuid_clinic;
      unset($item->uuid_clinic);

      $value1 = $item->expected;
      $value2 = $item->field_expected_field_expected_value2;
      $item->expected = [
        'value' => $value1 ? hedley_restful_timestamp_only_date($value1) : NULL,
        'value2' => $value2 ? hedley_restful_timestamp_only_date($value2) : NULL,

      ];
      unset($item->field_expected_field_expected_value2);
    }

    return $items;
  }

}
