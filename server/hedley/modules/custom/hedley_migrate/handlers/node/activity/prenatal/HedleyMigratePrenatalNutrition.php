<?php

/**
 * @file
 * Contains \HedleyMigratePrenatalNutrition.
 */

/**
 * Class HedleyMigratePrenatalNutrition.
 */
class HedleyMigratePrenatalNutrition extends HedleyMigrateNutritionMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'prenatal_nutrition';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_height',
        'field_weight',
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
        'field_height',
        'field_weight',
        'field_muac'
      ]
    );
  }

}
