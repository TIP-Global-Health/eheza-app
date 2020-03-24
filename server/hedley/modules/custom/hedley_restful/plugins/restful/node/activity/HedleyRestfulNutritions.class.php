<?php

/**
 * @file
 * Contains HedleyRestfulNutritions.
 */

/**
 * Class HedleyRestfulNutritions.
 */
class HedleyRestfulNutritions extends HedleyRestfulActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['nutrition_signs'] = [
      'property' => 'field_nutrition_signs',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    hedley_restful_join_field_to_query($query, 'node', 'field_nutrition_signs', FALSE);

    $query->addExpression("GROUP_CONCAT(DISTINCT field_nutrition_signs.field_nutrition_signs_value)", 'field_nutrition_signs');
    $query->groupBy('node.nid');
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->nutrition_signs = explode(',', $item->nutrition_signs);
    }
    return $items;
  }

}
