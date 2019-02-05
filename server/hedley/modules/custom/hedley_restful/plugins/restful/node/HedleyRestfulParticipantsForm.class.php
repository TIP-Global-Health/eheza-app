<?php

/**
 * @file
 * Contains HedleyRestfulParticipantsForm.
 */

/**
 * Class HedleyRestfulParticipantsForm.
 */
class HedleyRestfulParticipantsForm extends HedleyRestfulEntityBaseNode {

  /**
   * Return the type of the activity.
   *
   * @return string
   *   The type name.
   */
  protected static function getType() {
    return 'participant_form';
  }

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $field_names = ['field_kinyarwanda_title', 'field_kinyarwanda_body'];

    foreach ($field_names as $field_name) {
      $public_name = str_replace('field_', '', $field_name);
      $public_fields[$public_name] = [
        'property' => $field_name,
      ];
    }

    $public_fields['body'] = [
      'property' => 'body',
    ];

    return $public_fields;
  }

}
