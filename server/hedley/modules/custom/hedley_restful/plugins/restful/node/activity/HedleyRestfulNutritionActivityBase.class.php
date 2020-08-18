<?php

/**
 * @file
 * Contains HedleyRestfulNutritionActivityBase.
 */

/**
 * Class HedleyRestfulNutritionActivityBase.
 */
abstract class HedleyRestfulNutritionActivityBase extends HedleyRestfulIndividualActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['nutrition_encounter'] = [
      'property' => 'field_nutrition_encounter',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    hedley_restful_join_field_to_query($query, 'node', 'field_nutrition_encounter', FALSE);
    // Get the UUID of the Prenatal encounter.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_nutrition_encounter.field_nutrition_encounter_target_id", 'uuid_nutrition_encounter');

    return $query;
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->nutrition_encounter = $item->uuid_nutrition_encounter;
      unset($item->uuid_nutrition_encounter);
    }

    return $items;
  }

}
