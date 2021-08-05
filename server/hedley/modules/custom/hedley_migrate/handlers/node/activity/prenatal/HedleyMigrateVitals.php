<?php

/**
 * @file
 * Contains \HedleyMigrateVitals.
 */

/**
 * Class HedleyMigrateVitals.
 */
class HedleyMigrateVitals extends HedleyMigratePrenatalMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'vitals';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_sys',
        'field_dia',
        'field_heart_rate',
        'field_respiratory_rate',
        'field_body_temperature',
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
        'field_sys',
        'field_dia',
        'field_heart_rate',
        'field_respiratory_rate',
        'field_body_temperature',
      ]
    );
  }

}
