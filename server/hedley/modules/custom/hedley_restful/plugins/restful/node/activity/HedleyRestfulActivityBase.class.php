<?php

/**
 * @file
 * Contains HedleyRestfulActivityBase.
 */

/**
 * Class HedleyRestfulActivityBase.
 */
abstract class HedleyRestfulActivityBase extends HedleyRestfulEntityBaseNode {

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

}
