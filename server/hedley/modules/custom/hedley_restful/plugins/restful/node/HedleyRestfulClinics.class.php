<?php

/**
 * @file
 * Contains \HedleyRestfulClinics.
 */

/**
 * Class HedleyRestfulClinics.
 */
class HedleyRestfulClinics extends HedleyRestfulEntityBaseNode {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['type'] = [
      'callback' => 'static::getType',
    ];

    return $public_fields;
  }

  /**
   * Return the type of the entity.
   *
   * @return string
   *   The type name.
   */
  protected static function getType() {
    return 'clinic';
  }

}
