<?php

/**
 * @file
 * Contains \HedleyRestfulAcuteIllnessEncounter.
 */

/**
 * Class HedleyRestfulAcuteIllnessEncounter.
 */
class HedleyRestfulAcuteIllnessEncounter extends HedleyRestfulIndividualEncounter {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['acute_illness_diagnosis'] = [
      'property' => 'field_acute_illness_diagnosis',
    ];

    $public_fields['sequence_number'] = [
      'property' => 'field_sequence_number',
    ];

    $public_fields['ai_encounter_type'] = [
      'property' => 'field_ai_encounter_type',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    $field_names = [
      'field_acute_illness_diagnosis',
      'field_sequence_number',
      'field_ai_encounter_type',
    ];

    foreach ($field_names as $field_name) {
      hedley_general_join_field_to_query($query, 'node', $field_name, FALSE);
    }
  }

}
