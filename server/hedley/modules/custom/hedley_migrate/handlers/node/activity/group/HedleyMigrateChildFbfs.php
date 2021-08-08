<?php

/**
 * @file
 * Contains \HedleyMigrateChildFbfs.
 */

/**
 * Class HedleyMigrateChildFbfs.
 */
class HedleyMigrateChildFbfs extends HedleyMigrateGroupMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'child_fbf';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_distributed_amount',
        'field_distribution_notice',
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
        'field_distributed_amount',
        'field_distribution_notice',
      ]
    );
  }

}
