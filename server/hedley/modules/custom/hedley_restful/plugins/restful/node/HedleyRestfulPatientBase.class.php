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

    $public_fields['type'] = [
      'callback' => 'static::getType',
    ];

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

    foreach (array_keys(field_info_instances($this->getEntityType(), $this->getBundle())) as $field_name) {
      if (strpos($field_name, 'field_date') !== 0) {
        // Not a date field.
        continue;
      }
      $public_name = str_replace('field_', '', $field_name);
      $public_fields[$public_name] = [
        'property' => $field_name,
        'process_callbacks' => [
          [$this, 'convertTimestampToIso8601'],
        ],
      ];
    }

    return $public_fields;
  }

}
