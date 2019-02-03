<?php

/**
 * @file
 * Contains HedleyRestfulMothers.
 */

/**
 * Class HedleyRestfulMothers.
 */
class HedleyRestfulMothers extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['type'] = [
      'callback' => 'static::getType',
    ];

    $public_fields['first_name'] = [
      'property' => 'field_first_name',
    ];

    $public_fields['second_name'] = [
      'property' => 'field_second_name',
    ];

    $date_field_names = [];

    foreach ($date_field_names as $field_name) {
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

    $public_fields['ubudehe'] = [
      'property' => 'field_ubudehe',
    ];

    $public_fields['education_level'] = [
      'property' => 'field_education_level',
    ];

    $public_fields['profession'] = [
      'property' => 'field_profession',
    ];

    $public_fields['marital_status'] = [
      'property' => 'field_marital_status',
    ];

    $public_fields['hiv_status'] = [
      'property' => 'field_hiv_status',
    ];

    $public_fields['household_size'] = [
      'property' => 'field_household_size',
    ];

    $public_fields['phone_number'] = [
      'property' => 'field_phone_number',
    ];

    $public_fields['number_of_children'] = [
      'property' => 'field_number_of_children',
    ];

    $public_fields['children'] = [
      'property' => 'nid',
      'process_callbacks' => [
        [$this, 'getChildren'],
      ],
    ];

    return $public_fields;
  }

}
