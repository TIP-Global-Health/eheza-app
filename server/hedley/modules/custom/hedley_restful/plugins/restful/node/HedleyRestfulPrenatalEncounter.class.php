<?php

/**
 * @file
 * Contains \HedleyRestfulPrenatalEncounter.
 */

/**
 * Class HedleyRestfulPrenatalEncounter.
 */
class HedleyRestfulPrenatalEncounter extends HedleyRestfulIndividualEncounter {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['prenatal_encounter_type'] = [
      'property' => 'field_prenatal_encounter_type',
    ];

    $public_fields['prenatal_diagnosis'] = [
      'property' => 'field_prenatal_diagnosis',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    $field_names = [
      'field_prenatal_encounter_type',
      'field_prenatal_diagnosis',
    ];

    foreach ($field_names as $field_name) {
      hedley_general_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    $query->addExpression("GROUP_CONCAT(DISTINCT field_prenatal_diagnosis.field_prenatal_diagnosis_value)", 'field_prenatal_diagnosis');
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->prenatal_diagnosis = explode(',', $item->prenatal_diagnosis);
    }

    return $items;
  }

}
