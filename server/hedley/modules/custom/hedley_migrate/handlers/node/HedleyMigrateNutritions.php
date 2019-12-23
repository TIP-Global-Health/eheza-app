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
  protected $bundle = 'family_nutrition';

  /**
   * {@inheritdoc}
   */
  protected $simpleMultipleMappings = [
    'field_nutrition_signs',
  ];

}
