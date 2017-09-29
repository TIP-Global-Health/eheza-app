<?php

/**
 * @file
 * Contains HedleyRestfulActivities.
 */

/**
 * Class HedleyRestfulActivities.
 */
class HedleyRestfulActivities extends RestfulEntityBaseMultipleBundles {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['child'] = [
      'property' => 'field_child',
      'resource' => [
        // Bundle name.
        'child' => [
          // Resource name.
          'name' => 'children',
          'full_view' => TRUE,
        ],
      ],
    ];

    return $public_fields;
  }

}
