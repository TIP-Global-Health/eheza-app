<?php

/**
 * @file
 * Contains HedleyRestfulPhotos.
 */

/**
 * Class HedleyRestfulPhotos.
 */
class HedleyRestfulPhotos extends HedleyRestfulActivityBase {

  /**
   * Return the type of the activity.
   *
   * @return string
   *   The type name.
   */
  protected static function getType() {
    return 'photo';
  }

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['examination'] = [
      'property' => 'field_examination',
      'resource' => [
        // Bundle name.
        'examination' => [
          // Resource name.
          'name' => 'examinations',
          'full_view' => TRUE,
        ],
      ],
    ];

    $public_fields['photo'] = [
      'property' => 'field_photo',
    ];

    return $public_fields;
  }

}
