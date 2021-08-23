<?php

/**
 * @file
 * Contains \HedleyMigrateNutritionHeight.
 */

/**
 * Class HedleyMigrateNutritionHeight.
 */
class HedleyMigrateNutritionHeight extends HedleyMigrateNutritionMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'nutrition_height';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_height',
        'field_zscore_age',
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
        'field_height',
        'field_zscore_age',
      ]
    );
  }

}
