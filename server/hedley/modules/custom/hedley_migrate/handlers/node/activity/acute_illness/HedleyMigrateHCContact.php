<?php

/**
 * @file
 * Contains \HedleyMigrateHCContact.
 */

/**
 * Class HedleyMigrateHCContact.
 */
class HedleyMigrateHCContact extends HedleyMigrateAcuteIllnessMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'hc_contact';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_hc_contact',
        'field_hc_recommendation',
        'field_hc_response_time',
        'field_ambulance_arrival_time',
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
        'field_hc_contact',
        'field_hc_recommendation',
        'field_hc_response_time',
        'field_ambulance_arrival_time',
      ]
    );
  }

}
