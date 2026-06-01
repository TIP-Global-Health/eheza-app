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

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    hedley_general_join_field_to_query($query, 'node', 'field_language', FALSE);
    hedley_general_join_field_to_query($query, 'node', 'field_participant_form', FALSE);

    // Get the UUID of the Participant form.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_participant_form.field_participant_form_target_id", 'uuid_participant_form');
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->participant_form = $item->uuid_participant_form;
      unset($item->uuid_participant_form);
    }

    return $items;
  }

}
