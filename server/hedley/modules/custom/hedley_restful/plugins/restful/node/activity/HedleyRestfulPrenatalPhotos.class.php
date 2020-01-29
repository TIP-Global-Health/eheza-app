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
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

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
