<?php

/**
 * @file
 * Contains HedleyRestfulNutritions.
 */

/**
 * Class HedleyRestfulNutritions.
 */
class HedleyRestfulNutritions extends HedleyRestfulChildActivityBase {

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

  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    hedley_restful_join_field_to_query($query, 'node', 'field_nutrition_signs');
  }

  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    foreach ($items as &$row) {
      $row->nutrition_signs = $row->field_nutrition_signs;

      unset($row->field_nutrition_signs);
    }
    return $items;
  }
}
