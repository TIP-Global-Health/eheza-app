<?php

/**
 * @file
 * Contains HedleyRestfulMotherActivityBase.
 */

/**
 * Class HedleyRestfulMotherActivityBase.
 */
abstract class HedleyRestfulMotherActivityBase extends HedleyRestfulActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['mother'] = [
      'property' => 'field_mother',
      'resource' => [
        // Bundle name.
        'child' => [
          // Resource name.
          'name' => 'mothers',
          'full_view' => FALSE,
        ],
      ],
    ];

    return $public_fields;
  }

}
