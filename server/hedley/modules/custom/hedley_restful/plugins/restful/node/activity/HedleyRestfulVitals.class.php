<?php

/**
 * @file
 * Contains HedleyRestfulVitals.
 */

/**
 * Class HedleyRestfulVitals.
 */
class HedleyRestfulVitals extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $standard_fields_names = [
      'field_sys',
      'field_dia',
      'field_heart_rate',
      'field_respiratory_rate',
      'field_body_temperature',
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
