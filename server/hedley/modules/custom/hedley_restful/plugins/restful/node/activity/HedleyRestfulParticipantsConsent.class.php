<?php

/**
 * @file
 * Contains HedleyRestfulParticipantsConsent.
 */

/**
 * Class HedleyRestfulParticipantsConsent.
 */
class HedleyRestfulParticipantsConsent extends HedleyRestfulMotherActivityBase {

  /**
   * Return the type of the activity.
   *
   * @return string
   *   The type name.
   */
  protected static function getType() {
    return 'participant_consent';
  }

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['witness'] = [
      'property' => 'field_witness',
      'resource' => [
        'user' => [
          'name' => 'users',
          'full_view' => FALSE,
        ],
      ],
    ];

    $public_fields['language'] = [
      'property' => 'field_language',
    ];

    $public_fields['participant_form'] = [
      'property' => 'field_participant_form',
      'resource' => [
        'participant_form' => [
          'name' => 'participants-form',
          'full_view' => FALSE,
        ],
      ],
    ];

    return $public_fields;
  }

}
