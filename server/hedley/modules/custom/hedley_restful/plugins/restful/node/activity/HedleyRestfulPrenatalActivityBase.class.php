<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalActivityBase.
 */

/**
 * Class HedleyRestfulPrenatalActivityBase.
 */
abstract class HedleyRestfulPrenatalActivityBase extends HedleyRestfulActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['prenatal_encounter'] = [
      'property' => 'field_prenatal_encounter',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

}
