<?php

/**
 * @file
 * Contains \HedleyMigrateIsolation.
 */

/**
 * Class HedleyMigrateIsolation.
 */
class HedleyMigrateIsolation extends HedleyMigrateAcuteIllnessMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'isolation';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_isolation',
        'field_reason_for_not_isolating',
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
        'field_isolation',
        'field_reason_for_not_isolating',
      ]
    );
  }

}
