<?php

/**
 * @file
 * Contains \HedleyRestfulNCDEncounter.
 */

/**
 * Class HedleyRestfulNCDEncounter.
 */
class HedleyRestfulNCDEncounter extends HedleyRestfulIndividualEncounter {


  /**
   * A list of fields that are assigned single value.
   *
   * @var array
   */
  protected $fields = [];

  /**
   * A list of fields that are assigned multiple values.
   *
   * @var array
   */
  protected $multiFields = [
    'field_ncd_diagnoses',
  ];

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    foreach (array_merge($this->fields, $this->multiFields) as $field_name) {
      $public_name = str_replace('field_', '', $field_name);

      $public_fields[$public_name] = [
        'property' => $field_name,
      ];
    }

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
