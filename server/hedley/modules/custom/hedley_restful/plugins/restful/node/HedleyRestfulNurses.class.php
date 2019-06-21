<?php

/**
 * @file
 * Contains \HedleyRestfulNurses.
 */

/**
 * Class HedleyRestfulNurses.
 */
class HedleyRestfulNurses extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['health_centers'] = [
      'property' => 'field_health_centers',
      'sub_property' => 'field_uuid',
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
