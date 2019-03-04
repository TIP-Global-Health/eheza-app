<?php

/**
 * @file
 * Contains HedleyRestfulChildActivityBase.
 */

/**
 * Class HedleyRestfulChildActivityBase.
 */
abstract class HedleyRestfulChildActivityBase extends HedleyRestfulActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['child'] = [
      'property' => 'field_child',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

}
