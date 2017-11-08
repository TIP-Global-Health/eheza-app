<?php

/**
 * @file
 * Contains HedleyRestfulMeResource.
 */

/**
 * Class HedleyRestfulMeResource.
 */
class HedleyRestfulMeResource extends \RestfulEntityBaseUser {

  /**
   * {@inheritdoc}
   */
  protected $controllers = [
    '' => [
      \RestfulInterface::GET => 'viewEntity',
    ],
  ];

  /**
   * {@inheritdoc}
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

    unset($public_fields['self']);
    return $public_fields;
  }

  /**
   * Overrides \RestfulEntityBase::viewEntity().
   *
   * Always return the current user.
   */
  public function viewEntity($entity_id) {
    $account = $this->getAccount();
    return array(parent::viewEntity($account->uid));
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
