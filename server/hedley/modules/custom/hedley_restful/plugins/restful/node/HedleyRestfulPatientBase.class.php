<?php

/**
 * @file
 * Contains HedleyRestfulPatientBase.
 */

/**
 * Class HedleyRestfulPatientBase.
 */
class HedleyRestfulPatientBase extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['avatar'] = [
      'property' => 'field_avatar',
      'process_callbacks' => [
        [$this, 'imageProcess'],
      ],
      'image_styles' => ['large', 'patient-photo'],
    ];

    $standard_fields_names = [
      'field_first_name',
      'field_middle_name',
      'field_second_name',
      'field_national_id_number',
      'field_gender',
      'field_birth_date_estimated',
      'field_ubudehe',
      'field_province',
      'field_district',
      'field_sector',
      'field_cell',
      'field_village',
      'field_phone_number',
    ];

    foreach ($standard_fields_names as $field_name) {
      $public_name = str_replace('field_', '', $field_name);
      $public_fields[$public_name] = [
        'property' => $field_name,
      ];
    }

    $public_fields['date_birth'] = [
      'property' => 'field_date_birth',
      'process_callbacks' => [
        [$this, 'convertTimestampToYmd'],
      ],
    ];

    $public_fields['health_center'] = [
      'property' => 'field_health_center',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

}
