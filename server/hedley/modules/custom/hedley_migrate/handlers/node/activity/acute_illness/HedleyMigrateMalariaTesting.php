<?php

/**
 * @file
 * Contains \HedleyMigrateMalariaTesting.
 */

/**
 * Class HedleyMigrateMalariaTesting.
 */
class HedleyMigrateMalariaTesting extends HedleyMigrateAcuteIllnessMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'malaria_testing';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_malaria_rapid_test',
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
        'field_malaria_rapid_test',
      ]
    );
  }

}
