<?php

/**
 * @file
 * Contains \HedleyRestfulClinics.
 */

/**
 * Class HedleyRestfulClinics.
 */
class HedleyRestfulClinics extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['health_center'] = [
      'property' => 'field_health_center',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['group_type'] = [
      'property' => 'field_group_type',
    ];

    return $public_fields;
  }

}
