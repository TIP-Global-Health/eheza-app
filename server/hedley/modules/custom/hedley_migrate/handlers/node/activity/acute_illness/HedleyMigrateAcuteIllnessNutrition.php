<?php

/**
 * @file
 * Contains \HedleyMigrateAcuteIllnessNutrition.
 */

/**
 * Class HedleyMigrateAcuteIllnessNutrition.
 */
class HedleyMigrateAcuteIllnessNutrition extends HedleyMigrateAcuteIllnessMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'acute_illness_nutrition';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_nutrition_signs',
      ]
    );
  }

  /**
   * {@inheritdoc}
   */
  protected function simpleMultipleMappings() {
    $mappings = parent::simpleMultipleMappings();

    return array_merge(
      $mappings, [
        'field_nutrition_signs',
      ]
    );
  }

}
