<?php

/**
 * @file
 * Contains \HedleyMigrateNutritionWeight.
 */

/**
 * Class HedleyMigrateNutritionWeight.
 */
class HedleyMigrateNutritionWeight extends HedleyMigrateNutritionMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'nutrition_weight';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_weight',
        'field_bmi',
        'field_zscore_age',
        'field_zscore_length',
        'field_zscore_bmi',
      ]
    );
  }

  /**
   * {@inheritdoc}
   */
  protected function simpleMappings() {
    $mappings = parent::simpleMappings();

    return array_merge(
      $mappings, [
        'field_weight',
        'field_bmi',
        'field_zscore_age',
        'field_zscore_length',
        'field_zscore_bmi',
      ]
    );
  }

}
