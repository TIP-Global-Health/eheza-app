<?php

/**
 * @file
 * Contains \HedleyMigratePrenatalMeasurementBase.
 */

/**
 * Class HedleyMigratePrenatalMeasurementBase.
 */
abstract class HedleyMigratePrenatalMeasurementBase extends HedleyMigrateMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_prenatal_encounter',
      ]
    );
  }

  /**
   * HedleyMigratePrenatalMeasurementBase constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies[] = 'HedleyMigratePrenatalEncounters';

    $this
      ->addFieldMapping('field_prenatal_encounter', 'field_prenatal_encounter')
      ->sourceMigration('HedleyMigratePrenatalEncounters');
  }

}
