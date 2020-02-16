<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalPhotos.
 */

/**
 * Class HedleyRestfulPrenatalPhotos.
 */
class HedleyRestfulPrenatalPhotos extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_photo',
  ];

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    unset($public_fields['photo']);

    $public_fields['photo'] = [
      'property' => 'field_photo',
      'process_callbacks' => [
        [$this, 'imageProcess'],
      ],
      'image_styles' => ['patient-photo'],
    ];

    return $public_fields;
  }

}
