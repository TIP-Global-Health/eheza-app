<?php

/**
 * @file
 * Contains HedleyRestfulParticipantsConsent.
 */

/**
 * Class HedleyRestfulParticipantsConsent.
 */
class HedleyRestfulParticipantsConsent extends HedleyRestfulChildActivityBase {

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

    $field_names = ['field_mother', 'field_witness', 'field_language', 'field_participant_form'];

    foreach ($field_names as $field_name) {
      $public_name = str_replace('field_', '', $field_name);
      $public_fields[$public_name] = [
        'property' => $field_name,
      ];
    }

    return $public_fields;
  }

}
