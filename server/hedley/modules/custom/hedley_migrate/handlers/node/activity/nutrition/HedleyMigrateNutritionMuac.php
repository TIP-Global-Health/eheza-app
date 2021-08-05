<?php

/**
 * @file
 * Contains \HedleyMigrateNutritionMuac.
 */

/**
 * Class HedleyMigrateNutritionMuac.
 */
class HedleyMigrateNutritionMuac extends HedleyMigrateNutritionMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'nutrition_muac';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_muac',
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
        'field_muac',
      ]
    );
  }

}
