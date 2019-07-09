<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalNutritions.
 */

/**
 * Class HedleyRestfulPrenatalNutritions.
 */
class HedleyRestfulPrenatalNutritions extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $standard_fields_names = [
      'field_height',
      'field_weight',
      'field_bmi',
      'field_muac',
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
