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

    $public_fields['person'] = [
      'property' => 'field_person',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

}
