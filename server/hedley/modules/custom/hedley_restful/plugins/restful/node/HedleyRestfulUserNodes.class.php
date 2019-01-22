<?php

/**
 * @file
 * Contains \HedleyRestfulUserNodes.
 */

/**
 * Class HedleyRestfulUserNodes.
 */
class HedleyRestfulUserNodes extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['clinics'] = [
      'property' => 'field_clinics',
      'resource' => [
        'clinic' => [
          'name' => 'clinics',
          'full_view' => FALSE,
        ],
      ],
    ];

    $public_fields['pin_code'] = [
      'property' => 'field_pin_code',
    ];

    $public_fields['role'] = [
      'property' => 'field_role',
    ];

    return $public_fields;
  }

}
