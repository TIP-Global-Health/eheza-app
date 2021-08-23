<?php

/**
 * @file
 * Contains \HedleyMigrateTreatmentOngoing.
 */

/**
 * Class HedleyMigrateTreatmentOngoing.
 */
class HedleyMigrateTreatmentOngoing extends HedleyMigrateAcuteIllnessMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'treatment_ongoing';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_treatment_ongoing',
        'field_reason_for_not_taking',
        'field_missed_doses',
        'field_adverse_events',
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
        'field_reason_for_not_taking',
        'field_missed_doses',
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
        'field_treatment_ongoing',
        'field_adverse_events',
      ]
    );
  }

}
