<?php

/**
 * @file
 * Contains HedleyRestfulChildActivityBase.
 */

/**
 * Class HedleyRestfulChildActivityBase.
 */
abstract class HedleyRestfulChildActivityBase extends HedleyRestfulActivityBase {

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
          'full_view' => FALSE,
        ],
      ],
    ];

    return $public_fields;
  }

}
