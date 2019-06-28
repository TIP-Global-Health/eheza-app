<?php

/**
 * @file
 * Contains HedleyRestfulObstetricalExams.
 */

/**
 * Class HedleyRestfulObstetricalExams.
 */
class HedleyRestfulObstetricalExams extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $standard_fields_names = [
      'field_fundal_height',
      'field_fetal_presentation',
      'field_fetal_movement',
      'field_fetal_heart_rate',
      'field_c_section_scar',
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
