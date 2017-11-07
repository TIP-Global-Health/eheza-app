<?php

/**
 * @file
 * Contains \HedelyRestfulUser.
 */

/**
 * Class HedelyRestfulUser.
 */
class HedleyRestfulUser extends \RestfulEntityBaseUser {

  /**
   * Overrides \RestfulEntityBaseUser::publicFieldsInfo().
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['avatar_url'] = [
      'property' => 'field_avatar',
      'process_callbacks' => array(
        array($this, 'imageProcess'),
      ),
      'image_styles' => ['large'],
    ];

    $public_fields['clinics'] = [
      'property' => 'field_clinics',
      'resource' => [
        'clinic' => [
          'name' => 'clinics',
          'full_view' => FALSE,
        ],
      ],
    ];

    return $public_fields;
  }

  /**
   * Returns only the required image.
   *
   * @return string
   *   The src of the image style.
   */
  public function imageProcess($value) {
    return $value['image_styles']['large'];
  }

}
