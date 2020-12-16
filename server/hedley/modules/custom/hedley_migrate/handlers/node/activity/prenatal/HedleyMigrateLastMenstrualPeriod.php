<?php

/**
 * @file
 * Contains \HedleyMigrateLastMenstrualPeriod.
 */

/**
 * Class HedleyMigrateLastMenstrualPeriod.
 */
class HedleyMigrateLastMenstrualPeriod extends HedleyMigratePrenatalMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'last_menstrual_period';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_last_menstrual_period',
        'field_confident',
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
        'field_confident',
      ]
    );
  }

  /**
   * HedleyMigrateLastMenstrualPeriod constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this
      ->addFieldMapping('field_last_menstrual_period', 'field_last_menstrual_period')
      ->callbacks([$this, 'dateProcess']);
  }

}
