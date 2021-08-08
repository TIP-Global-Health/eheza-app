<?php

/**
 * @file
 * Contains \HedleyMigrateTreatmentHistory.
 */

/**
 * Class HedleyMigrateTreatmentHistory.
 */
class HedleyMigrateTreatmentHistory extends HedleyMigrateAcuteIllnessMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'treatment_history';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_treatment_history',
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
        'field_treatment_history',
      ]
    );
  }

}
