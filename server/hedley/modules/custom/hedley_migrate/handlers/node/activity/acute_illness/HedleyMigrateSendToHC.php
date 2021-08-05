<?php

/**
 * @file
 * Contains \HedleyMigrateSendToHC.
 */

/**
 * Class HedleyMigrateSendToHC.
 */
class HedleyMigrateSendToHC extends HedleyMigrateAcuteIllnessMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'send_to_hc';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_send_to_hc',
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
        'field_send_to_hc',
      ]
    );
  }

}
