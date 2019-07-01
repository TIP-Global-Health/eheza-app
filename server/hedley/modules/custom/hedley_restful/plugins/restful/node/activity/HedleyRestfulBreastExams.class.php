<?php

/**
 * @file
 * Contains HedleyRestfulBreastExams.
 */

/**
 * Class HedleyRestfulBreastExams.
 */
class HedleyRestfulBreastExams extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $standard_fields_names = [
      'field_breast',
      'field_breast_self_exam',
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
