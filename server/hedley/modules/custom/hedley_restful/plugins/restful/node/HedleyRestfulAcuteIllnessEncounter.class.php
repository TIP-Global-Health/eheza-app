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

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    $field_names = [
      'field_acute_illness_diagnosis',
    ];

    foreach ($field_names as $field_name) {
      hedley_restful_join_field_to_query($query, 'node', $field_name, FALSE);
    }
  }

}
