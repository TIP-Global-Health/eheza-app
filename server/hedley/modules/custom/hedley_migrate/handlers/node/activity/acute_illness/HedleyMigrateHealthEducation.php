<?php

/**
 * @file
 * Contains \HedleyMigrateHealthEducation.
 */

/**
 * Class HedleyMigrateHealthEducation.
 */
class HedleyMigrateHealthEducation extends HedleyMigrateAcuteIllnessMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'health_education';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_health_education_signs',
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
        'field_health_education_signs',
      ]
    );
  }

}
