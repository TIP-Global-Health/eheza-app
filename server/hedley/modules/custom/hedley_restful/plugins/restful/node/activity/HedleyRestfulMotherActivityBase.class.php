<?php

/**
 * @file
 * Contains HedleyRestfulMotherActivityBase.
 */

/**
 * Class HedleyRestfulMotherActivityBase.
 */
abstract class HedleyRestfulMotherActivityBase extends HedleyRestfulActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['mother'] = [
      'property' => 'field_mother',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

}
