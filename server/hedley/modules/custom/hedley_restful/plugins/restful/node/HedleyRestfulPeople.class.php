<?php

/**
 * @file
 * Contains HedleyRestfulPeople.
 */

/**
 * Class HedleyRestfulPeople.
 */
class HedleyRestfulPeople extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $standard_fields_names = [
      'field_birth_date_estimated',
      'field_cell',
      'field_district',
      'field_education_level',
      'field_first_name',
      'field_gender',
      'field_marital_status',
      'field_middle_name',
      'field_national_id_number',
      'field_phone_number',
      'field_profession',
      'field_province',
      'field_second_name',
      'field_sector',
      'field_ubudehe',
      'field_village',
    ];

    foreach ($standard_fields_names as $field_name) {
      $public_name = str_replace('field_', '', $field_name);
      $public_fields[$public_name] = [
        'property' => $field_name,
      ];
    }

    $public_fields['photo'] = [
      'property' => 'field_photo',
      'process_callbacks' => [
        [$this, 'imageProcess'],
      ],
      'image_styles' => ['patient-photo'],
    ];

    $public_fields['birth_date'] = [
      'property' => 'field_birth_date',
      'process_callbacks' => [
        [$this, 'convertTimestampToYmd'],
      ],
    ];

    $public_fields['clinic'] = [
      'property' => 'field_clinic',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

}
