<?php

/**
 * @file
 * Contains \HedleyMigrateAcuteIllnessVitals.
 */

/**
 * Class HedleyMigrateAcuteIllnessVitals.
 */
class HedleyMigrateAcuteIllnessVitals extends HedleyMigrateAcuteIllnessMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'acute_illness_vitals';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_respiratory_rate',
        'field_body_temperature',
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
        'field_respiratory_rate',
        'field_body_temperature',
      ]
    );
  }

}
