<?php

/**
 * @file
 * Contains \HedleyMigrateObstetricHistory.
 */

/**
 * Class HedleyMigrateObstetricHistory.
 */
class HedleyMigrateObstetricHistory extends HedleyMigratePrenatalMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'obstetric_history';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_currently_pregnant',
        'field_term_pregnancy',
        'field_preterm_pregnancy',
        'field_stillbirths_at_term',
        'field_stillbirths_preterm',
        'field_abortions',
        'field_live_children',
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
        'field_currently_pregnant',
        'field_term_pregnancy',
        'field_preterm_pregnancy',
        'field_stillbirths_at_term',
        'field_stillbirths_preterm',
        'field_abortions',
        'field_live_children',
      ]
    );
  }

}
