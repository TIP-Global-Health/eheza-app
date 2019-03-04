<?php

/**
 * @file
 * Contains HedleyRestfulMothers.
 */

/**
 * Class HedleyRestfulMothers.
 */
class HedleyRestfulMothers extends HedleyRestfulPatientBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $standard_fields_names = [
      'field_education_level',
      'field_gender',
      'field_profession',
      'field_marital_status',
      'field_hiv_status',
      'field_household_size',
      'field_number_of_children',
    ];

    foreach ($standard_fields_names as $field_name) {
      $public_name = str_replace('field_', '', $field_name);
      $public_fields[$public_name] = [
        'property' => $field_name,
      ];
    }

    $public_fields['relation'] = [
      'property' => 'field_relationship',
    ];

    $public_fields['ubudehe'] = [
      'property' => 'field_ubudehe',
    ];

    return $public_fields;
  }

}
