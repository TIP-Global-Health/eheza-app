<?php

/**
 * @file
 * Contains HedleyRestfulObstetricHistoriesStep2.
 */

/**
 * Class HedleyRestfulObstetricHistoriesStep2.
 */
class HedleyRestfulObstetricHistoriesStep2 extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $standard_fields_names = [
      'field_c_sections',
      'field_c_section_reason',
      'field_obstetric_history',
      'field_previous_delivery',
      'field_previous_delivery_period',
    ];

    foreach ($standard_fields_names as $field_name) {
      $public_name = str_replace('field_', '', $field_name);
      $public_fields[$public_name] = [
        'property' => $field_name,
      ];
    }

    return $public_fields;
  }

}
