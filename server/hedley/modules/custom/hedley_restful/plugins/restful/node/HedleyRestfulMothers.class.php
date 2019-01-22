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

    return $public_fields;
  }

}
