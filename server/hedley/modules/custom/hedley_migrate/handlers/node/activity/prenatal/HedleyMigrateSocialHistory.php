<?php

/**
 * @file
 * Contains \HedleyMigrateSocialHistory.
 */

/**
 * Class HedleyMigrateSocialHistory.
 */
class HedleyMigrateSocialHistory extends HedleyMigratePrenatalMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'social_history';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_social_history',
        'field_partner_hiv_testing',
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
        'field_social_history',
        'field_partner_hiv_testing',
      ]
    );
  }

}
