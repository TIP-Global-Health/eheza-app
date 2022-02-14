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

    $public_fields['prenatal_diagnoses'] = [
      'property' => 'field_prenatal_diagnoses',
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
      'field_prenatal_diagnoses',
    ];

    foreach ($field_names as $field_name) {
      hedley_general_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    $query->addExpression("GROUP_CONCAT(DISTINCT field_prenatal_diagnoses.field_prenatal_diagnoses_value)", 'field_prenatal_diagnoses');
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->prenatal_diagnoses = explode(',', $item->prenatal_diagnoses);
    }

    return $items;
  }

}
