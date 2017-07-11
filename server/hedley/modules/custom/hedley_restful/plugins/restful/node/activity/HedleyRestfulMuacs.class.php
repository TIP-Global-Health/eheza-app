<?php

/**
 * @file
 * Contains HedleyRestfulMuacs.
 */

/**
 * Class HedleyRestfulMuacs.
 */
class HedleyRestfulMuacs extends HedleyRestfulActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['muac'] = [
      'property' => 'field_muac',
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
    return 'muac';
  }

}
