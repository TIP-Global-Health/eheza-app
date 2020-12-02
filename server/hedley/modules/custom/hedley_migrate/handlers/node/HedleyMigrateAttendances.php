<?php

/**
 * @file
 * Contains \HedleyMigrateAttendances.
 */

/**
 * Class HedleyMigrateAttendances.
 */
class HedleyMigrateAttendances extends HedleyMigrateGroupMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'attendance';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_attended',
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
        'field_attended',
      ]
    );
  }

}
