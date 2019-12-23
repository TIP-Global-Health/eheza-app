<?php

/**
 * @file
 * Contains \HedleyMigrateHeights.
 */

/**
 * Class HedleyMigrateHeights.
 */
class HedleyMigrateHeights extends HedleyMigrateMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'height';

  /**
   * {@inheritdoc}
   */
  protected $simpleMappings = [
    'field_height',
    'field_zscore_age',
  ];

}
