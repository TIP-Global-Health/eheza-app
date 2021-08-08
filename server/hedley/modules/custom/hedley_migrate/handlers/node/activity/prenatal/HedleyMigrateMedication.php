<?php

/**
 * @file
 * Contains \HedleyMigrateMedication.
 */

/**
 * Class HedleyMigrateMedication.
 */
class HedleyMigrateMedication extends HedleyMigratePrenatalMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'medication';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_medication',
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
        'field_medication',
      ]
    );
  }

}
