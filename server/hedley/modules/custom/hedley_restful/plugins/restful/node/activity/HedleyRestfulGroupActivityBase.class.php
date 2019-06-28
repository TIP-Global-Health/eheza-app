<?php

/**
 * @file
 * Contains HedleyRestfulGroupActivityBase.
 */

/**
 * Class HedleyRestfulGroupActivityBase.
 */
abstract class HedleyRestfulGroupActivityBase extends HedleyRestfulActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['session'] = [
      'property' => 'field_session',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

}
