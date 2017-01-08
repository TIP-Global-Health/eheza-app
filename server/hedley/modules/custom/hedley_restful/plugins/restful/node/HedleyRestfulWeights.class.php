<?php

/**
 * @file
 * Contains HedleyRestfulWeights.
 */

/**
 * Class HedleyRestfulWeights.
 */
class HedleyRestfulWeights extends HedleyRestfulEntityBaseNode {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $field_names = ['field_date'];

    foreach ($field_names as $field_name) {
      $public_name = str_replace('field_', '', $field_name);
      $public_fields[$public_name] = [
        'property' => $field_name,
      ];
    }

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
