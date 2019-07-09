<?php

/**
 * @file
 * Contains HedleyRestfulObstetricHistories.
 */

/**
 * Class HedleyRestfulObstetricHistories.
 */
class HedleyRestfulObstetricHistories extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $standard_fields_names = [
      'field_currently_pregnant',
      'field_term_pregnancy',
      'field_preterm_pregnancy',
      'field_stillbirths_at_term',
      'field_stillbirths_preterm',
      'field_abortions',
      'field_live_children',
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
