<?php

/**
 * @file
 * Contains \HedleyMigrateWeights.
 */

/**
 * Class HedleyMigrateWeights.
 */
class HedleyMigrateWeights extends HedleyMigrateMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'weight';

  /**
   * {@inheritdoc}
   */
  protected $simpleMappings = [
    'field_weight',
    'field_bmi',
    'field_zscore_age',
    'field_zscore_length',
    'field_zscore_bmi,'
  ];

}
