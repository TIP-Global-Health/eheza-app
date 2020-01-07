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

}
