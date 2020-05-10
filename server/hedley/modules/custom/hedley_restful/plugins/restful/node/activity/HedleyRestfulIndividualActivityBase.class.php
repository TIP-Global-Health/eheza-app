<?php

/**
 * @file
 * Contains HedleyRestfulIndividualActivityBase.
 */

/**
 * Class HedleyRestfulIndividualActivityBase.
 */
abstract class HedleyRestfulIndividualActivityBase extends HedleyRestfulActivityBase {

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
  protected $multiFields = [];

  /**
   * A list of fields that are dates. This is a sub list of $fields.
   *
   * @var array
   */
  protected $dateFields = [];

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
      hedley_restful_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    foreach ($this->multiFields as $field_name) {
      $query->addExpression("GROUP_CONCAT(DISTINCT $field_name.{$field_name}_value)", $field_name);
    }

    $query->groupBy('node.nid');

    return $query;
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

      foreach ($this->dateFields as $field_name) {
        $public_name = str_replace('field_', '', $field_name);
        $date = explode(' ', $item->{$public_name});
        $item->{$public_name} = !empty($date[0]) ? $date[0] : NULL;
      }
    }

    return $items;
  }

}
