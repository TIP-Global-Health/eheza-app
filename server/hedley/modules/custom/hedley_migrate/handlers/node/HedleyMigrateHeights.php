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
  protected $csvColumns = [
    'id',
    'field_person',
    'field_date_measured',
    'field_nurse',
    'field_session',
    'field_height',
    'field_zscore_age',
  ];

  /**
   * {@inheritdoc}
   */
  protected $simpleMappings = [
    'field_height',
    'field_zscore_age',
  ];

}
