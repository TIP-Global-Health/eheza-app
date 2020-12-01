<?php

/**
 * @file
 * Contains \HedleyMigrateNutritions.
 */

/**
 * Class HedleyMigrateNutritions.
 */
class HedleyMigrateNutritions extends HedleyMigrateMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'nutrition';

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
  protected function simpleMappings() {
    $mappings = parent::simpleMappings();

    return array_merge(
      $mappings, [
        'field_nutrition_signs',
      ]
    );
  }

}
