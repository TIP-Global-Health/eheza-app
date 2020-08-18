<?php

/**
 * @file
 * Contains HedleyRestfulLactations.
 */

/**
 * Class HedleyRestfulLactations.
 */
class HedleyRestfulLactations extends HedleyRestfulGroupActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['lactation_signs'] = [
      'property' => 'field_lactation_signs',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    hedley_restful_join_field_to_query($query, 'node', 'field_lactation_signs', FALSE);

    $query->addExpression("GROUP_CONCAT(DISTINCT field_lactation_signs.field_lactation_signs_value)", 'field_lactation_signs');
    $query->groupBy('node.nid');
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->lactation_signs = explode(',', $item->lactation_signs);
    }

    return $items;
  }

}
