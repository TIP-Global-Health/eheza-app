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
  protected $simpleMultipleMappings = [
    'field_nutrition_signs',
  ];

}
