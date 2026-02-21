<?php

/**
 * @file
 * Contains HedleyRestfulFamilyNutritionActivityBase.
 */

/**
 * Class HedleyRestfulFamilyNutritionActivityBase.
 */
abstract class HedleyRestfulFamilyNutritionActivityBase extends HedleyRestfulActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['family_nutrition_encounter'] = [
      'property' => 'field_family_nutrition_encounter',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    hedley_general_join_field_to_query($query, 'node', 'field_family_nutrition_encounter', FALSE);
    // Get the UUID of the Prenatal encounter.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_family_nutrition_encounter.field_family_nutrition_encounter_target_id", 'uuid_family_nutrition_encounter');

    return $query;
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->family_nutrition_encounter = $item->uuid_family_nutrition_encounter;
      unset($item->uuid_family_nutrition_encounter);
    }

    return $items;
  }

}
