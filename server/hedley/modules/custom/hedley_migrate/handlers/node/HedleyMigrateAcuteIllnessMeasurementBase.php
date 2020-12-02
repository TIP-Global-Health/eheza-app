<?php

/**
 * @file
 * Contains \HedleyMigrateAcuteIllnessMeasurementBase.
 */

/**
 * Class HedleyMigrateAcuteIllnessMeasurementBase.
 */
abstract class HedleyMigrateAcuteIllnessMeasurementBase extends HedleyMigrateMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_acute_illness_encounter',
      ]
    );
  }

  /**
   * HedleyMigrateAcuteIllnessMeasurementBase constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigrateAcuteIllnessEncounters',
    ];

    $this
      ->addFieldMapping('field_acute_illness_encounter', 'field_acute_illness_encounter')
      ->sourceMigration('HedleyMigrateAcuteIllnessEncounters');
  }

}
