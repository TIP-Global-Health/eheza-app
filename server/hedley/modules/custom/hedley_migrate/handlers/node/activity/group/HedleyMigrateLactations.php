<?php

/**
 * @file
 * Contains \HedleyMigrateLactations.
 */

/**
 * Class HedleyMigrateLactations.
 */
class HedleyMigrateLactations extends HedleyMigrateGroupMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'lactation';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_lactation_signs',
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
        'field_lactation_signs',
      ]
    );
  }

}
