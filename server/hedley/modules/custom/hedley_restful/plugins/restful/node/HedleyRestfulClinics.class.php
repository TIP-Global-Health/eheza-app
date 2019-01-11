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
      'resource' => [
        'health_center' => [
          'name' => 'health_centers',
          'full_view' => FALSE,
        ],
      ],
    ];

    return $public_fields;
  }

}
