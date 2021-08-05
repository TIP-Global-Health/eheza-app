<?php

/**
 * @file
 * Contains \HedleyMigrateObstetricHistoryStep2.
 */

/**
 * Class HedleyMigrateObstetricHistoryStep2.
 */
class HedleyMigrateObstetricHistoryStep2 extends HedleyMigratePrenatalMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'obstetric_history_step2';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_c_sections',
        'field_c_section_reason',
        'field_previous_delivery_period',
        'field_obstetric_history',
        'field_previous_delivery',
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
        'field_c_sections',
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
        'field_c_section_reason',
        'field_previous_delivery_period',
        'field_obstetric_history',
        'field_previous_delivery',
      ]
    );
  }

}
