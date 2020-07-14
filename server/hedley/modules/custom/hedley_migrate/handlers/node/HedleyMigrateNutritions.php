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
  protected $csvColumns = [
    'id',
    'field_person',
    'field_date_measured',
    'field_nurse',
    'field_session',
    'created',
    'field_nutrition_signs',
  ];

  /**
   * {@inheritdoc}
   */
  protected $simpleMultipleMappings = [
    'field_nutrition_signs',
  ];

}
