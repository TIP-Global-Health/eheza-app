<?php

/**
 * @file
 * Contains HedleyRestfulWeights.
 */

/**
 * Class HedleyRestfulWeights.
 */
class HedleyRestfulMothers extends HedleyRestfulEntityBaseNode {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['type'] = [
      'callback' => 'static::getType',
    ];

    $field_names = [];

    foreach ($field_names as $field_name) {
      $public_name = str_replace('field_', '', $field_name);
      $public_fields[$public_name] = [
        'property' => $field_name,
      ];
    }

    $public_fields['avatar'] = [
      'property' => 'field_avatar',
      'process_callbacks' => [
        [$this, 'imageProcess'],
      ],
      'image_styles' => ['large', 'thumbnail'],
    ];

    $public_fields['children'] = [
      'property' => 'field_children',
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

  /**
   * Return the type of the patient.
   *
   * @return string
   *   The type name.
   */
  protected static function getType() {
    return 'mother';
  }

}
