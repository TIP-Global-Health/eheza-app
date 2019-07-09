<?php

/**
 * @file
 * Contains HedleyRestfulCorePhysicalExames.
 */

/**
 * Class HedleyRestfulCorePhysicalExams.
 */
class HedleyRestfulCorePhysicalExams extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $standard_fields_names = [
      'field_head_hair',
      'field_eyes',
      'field_neck',
      'field_heart',
      'field_lungs',
      'field_abdomen',
      'field_hands',
      'field_legs',
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
