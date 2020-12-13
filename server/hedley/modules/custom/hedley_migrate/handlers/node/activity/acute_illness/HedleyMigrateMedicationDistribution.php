<?php

/**
 * @file
 * Contains \HedleyMigrateMedicationDistribution.
 */

/**
 * Class HedleyMigrateMedicationDistribution.
 */
class HedleyMigrateMedicationDistribution extends HedleyMigrateAcuteIllnessMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'medication_distribution';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_prescribed_medication',
        'field_non_administration_reason',
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
        'field_prescribed_medication',
        'field_non_administration_reason',
      ]
    );
  }

}
