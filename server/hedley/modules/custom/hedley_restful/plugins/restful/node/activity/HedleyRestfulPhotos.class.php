<?php

/**
 * @file
 * Contains HedleyRestfulPhotos.
 */

/**
 * Class HedleyRestfulPhotos.
 */
class HedleyRestfulPhotos extends HedleyRestfulGroupActivityBase {

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
