<?php

/**
 * @file
 * Contains \HedleyRestfulNutritionEncounter.
 */

/**
 * Class HedleyRestfulNutritionEncounter.
 */
class HedleyRestfulNutritionEncounter extends HedleyRestfulIndividualEncounter {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['nutrition_encounter_type'] = [
      'property' => 'field_nutrition_encounter_type',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    $field_names = [
      'field_nutrition_encounter_type',
    ];

    foreach ($field_names as $field_name) {
      hedley_general_join_field_to_query($query, 'node', $field_name, FALSE);
    }
  }

}
