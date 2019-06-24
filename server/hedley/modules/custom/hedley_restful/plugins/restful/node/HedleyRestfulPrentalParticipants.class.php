<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalParticipants.
 */

/**
 * Class HedleyRestfulPrenatalParticipants.
 */
class HedleyRestfulPrenatalParticipants extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['person'] = [
      'property' => 'field_person',
      'sub_property' => 'field_uuid',
    ];

    // The label is decorative only.
    unset($public_fields['label']);

    return $public_fields;
  }

}
