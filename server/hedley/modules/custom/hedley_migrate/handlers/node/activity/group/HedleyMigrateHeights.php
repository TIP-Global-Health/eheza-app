<?php

/**
 * @file
 * Contains \HedleyMigrateHeights.
 */

/**
 * Class HedleyMigrateHeights.
 */
class HedleyMigrateHeights extends HedleyMigrateGroupMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'height';

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
