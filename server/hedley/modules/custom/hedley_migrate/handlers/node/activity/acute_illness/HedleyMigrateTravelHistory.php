<?php

/**
 * @file
 * Contains \HedleyMigrateTravelHistory.
 */

/**
 * Class HedleyMigrateTravelHistory.
 */
class HedleyMigrateTravelHistory extends HedleyMigrateAcuteIllnessMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'travel_history';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_travel_history',
      ]
    );
  }

  /**
   * {@inheritdoc}
   */
  protected function simpleMultipleMappings() {
    $mappings = parent::simpleMultipleMappings();

    return array_merge(
      $mappings, [
        'field_travel_history',
      ]
    );
  }

}
