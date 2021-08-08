<?php

/**
 * @file
 * Contains \HedleyMigrateCall114.
 */

/**
 * Class HedleyMigrateCall114.
 */
class HedleyMigrateCall114 extends HedleyMigrateAcuteIllnessMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'call_114';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_114_contact',
        'field_114_recommendation',
        'field_site_recommendation',
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
        'field_114_contact',
        'field_114_recommendation',
        'field_site_recommendation',
      ]
    );
  }

}
