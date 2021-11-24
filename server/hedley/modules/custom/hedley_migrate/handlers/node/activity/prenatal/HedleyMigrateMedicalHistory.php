<?php

/**
 * @file
 * Contains \HedleyMigrateMedicalHistory.
 */

/**
 * Class HedleyMigrateMedicalHistory.
 */
class HedleyMigrateMedicalHistory extends HedleyMigratePrenatalMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'medical_history';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_medical_history',
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
        'field_medical_history',
      ]
    );
  }

}
