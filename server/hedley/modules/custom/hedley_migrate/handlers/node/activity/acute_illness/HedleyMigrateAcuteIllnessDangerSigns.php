<?php

/**
 * @file
 * Contains \HedleyMigrateAcuteIllnessDangerSigns.
 */

/**
 * Class HedleyMigrateAcuteIllnessDangerSigns.
 */
class HedleyMigrateAcuteIllnessDangerSigns extends HedleyMigrateAcuteIllnessMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'acute_illness_danger_signs';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_acute_illness_danger_signs',
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
        'field_acute_illness_danger_signs',
      ]
    );
  }

}
