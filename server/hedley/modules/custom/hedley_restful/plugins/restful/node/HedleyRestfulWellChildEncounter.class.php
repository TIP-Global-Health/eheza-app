<?php

/**
 * @file
 * Contains \HedleyRestfulWellChildEncounter.
 */

/**
 * Class HedleyRestfulWellChildEncounter.
 */
class HedleyRestfulWellChildEncounter extends HedleyRestfulIndividualEncounter {


  /**
   * A list of fields that are assigned single value.
   *
   * @var array
   */
  protected $fields = [
    'field_well_child_encounter_type',
  ];

  /**
   * A list of fields that are assigned multiple values.
   *
   * @var array
   */
  protected $multiFields = [
    'field_encounter_notes',
    'field_encounter_warnings',
  ];

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['well_child_encounter_type'] = [
      'property' => 'field_well_child_encounter_type',
    ];

    $public_fields['encounter_notes'] = [
      'property' => 'field_encounter_notes',
    ];

    $public_fields['encounter_warnings'] = [
      'property' => 'field_encounter_warnings',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    foreach (array_merge($this->fields, $this->multiFields) as $field_name) {
      hedley_general_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    foreach ($this->multiFields as $field_name) {
      $query->addExpression("GROUP_CONCAT(DISTINCT $field_name.{$field_name}_value)", $field_name);
    }

    $query->groupBy('node.nid');
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      foreach ($this->multiFields as $field_name) {
        $public_name = str_replace('field_', '', $field_name);
        $item->{$public_name} = explode(',', $item->{$public_name});
      }
    }

    return $items;
  }

}
