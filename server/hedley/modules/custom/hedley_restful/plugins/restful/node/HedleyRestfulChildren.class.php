<?php

/**
 * @file
 * Contains HedleyRestfulChildren.
 */

/**
 * Class HedleyRestfulChildren.
 */
class HedleyRestfulChildren extends HedleyRestfulPatientBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $standard_fields_names = [
      'field_mode_of_delivery',
      'field_mother_name',
      'field_mother_national_id',
      'field_father_name',
      'field_father_national_id',
      'field_caregiver_name_',
      'field_caregiver_national_id',
    ];

    foreach ($standards_field_names as $field_name) {
      $public_name = str_replace('field_', '', $field_name);
      $public_fields[$public_name] = [
        'property' => $field_name,
      ];
    }

    $public_fields['avatar'] = [
      'property' => 'field_avatar',
      'process_callbacks' => [
        [$this, 'imageProcess'],
      ],
      'image_styles' => ['large', 'patient-photo'],
    ];

    $public_fields['mother'] = [
      'property' => 'field_mother',
      'resource' => [
        'mother' => [
          'name' => 'mothers',
          'full_view' => FALSE,
        ],
      ],
    ];

    return $public_fields;
  }

}
