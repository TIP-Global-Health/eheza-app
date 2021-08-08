<?php

/**
 * @file
 * Contains \HedleyMigrateAcuteIllnessMuac.
 */

/**
 * Class HedleyMigrateAcuteIllnessMuac.
 */
class HedleyMigrateAcuteIllnessMuac extends HedleyMigrateAcuteIllnessMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'acute_illness_muac';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_muac',
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
        'field_muac',
      ]
    );
  }

}
