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

    $public_fields['examination'] = [
      'property' => 'field_examination',
      'resource' => [
        // Bundle name.
        'examination' => [
          // Resource name.
          'name' => 'examinations',
          'full_view' => FALSE,
        ],
      ],
    ];

    return $public_fields;
  }

}
