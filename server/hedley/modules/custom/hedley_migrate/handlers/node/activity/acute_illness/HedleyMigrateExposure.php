<?php

/**
 * @file
 * Contains \HedleyMigrateExposure.
 */

/**
 * Class HedleyMigrateExposure.
 */
class HedleyMigrateExposure extends HedleyMigrateAcuteIllnessMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'exposure';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_exposure',
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
        'field_exposure',
      ]
    );
  }

}
