<?php

/**
 * @file
 * Contains \HedleyMigrateDangerSigns.
 */

/**
 * Class HedleyMigrateDangerSigns.
 */
class HedleyMigrateDangerSigns extends HedleyMigratePrenatalMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'danger_signs';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_danger_signs',
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
        'field_danger_signs',
      ]
    );
  }

}
