<?php

/**
 * @file
 * Contains HedleyRestfulHeights.
 */

/**
 * Class HedleyRestfulHeights.
 */
class HedleyRestfulHeights extends HedleyRestfulChildActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['height'] = [
      'property' => 'field_height',
    ];

    $public_fields['z_score_age'] = [
      'property' => 'field_z_score_age',
    ];

    return $public_fields;
  }

  /**
   * Return the type of the activity.
   *
   * @return string
   *   The type name.
   */
  protected static function getType() {
    return 'height';
  }

}
