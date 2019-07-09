<?php

/**
 * @file
 * Contains HedleyRestfulParticipantsConsent.
 */

/**
 * Class HedleyRestfulParticipantsConsent.
 */
class HedleyRestfulParticipantsConsent extends HedleyRestfulGroupActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['language'] = [
      'property' => 'field_language',
    ];

    $public_fields['participant_form'] = [
      'property' => 'field_participant_form',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

}
