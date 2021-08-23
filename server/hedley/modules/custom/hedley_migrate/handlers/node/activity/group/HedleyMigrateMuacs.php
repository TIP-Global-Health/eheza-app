<?php

/**
 * @file
 * Contains \HedleyMigrateMuacs.
 */

/**
 * Class HedleyMigrateMuacs.
 */
class HedleyMigrateMuacs extends HedleyMigrateGroupMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'muac';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
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
        'field_muac',
      ]
    );
  }

}
