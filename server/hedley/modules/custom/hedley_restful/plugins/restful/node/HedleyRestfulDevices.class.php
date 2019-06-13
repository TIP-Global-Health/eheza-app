<?php

/**
 * @file
 * Contains \HedleyRestfulDevices.
 */

/**
 * Class HedleyRestfulDevices.
 */
class HedleyRestfulDevices extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['uid'] = [
      'property' => 'uid',
    ];

    return $public_fields;
  }

}
