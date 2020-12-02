<?php

/**
 * @file
 * Contains \HedleyMigrateGroupMeasurementBase.
 */

/**
 * Class HedleyMigrateGroupMeasurementBase.
 */
abstract class HedleyMigrateGroupMeasurementBase extends HedleyMigrateMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_session',
      ]
    );
  }

  /**
   * HedleyMigrateGroupMeasurementBase constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigrateSessions',
    ];

    $this
      ->addFieldMapping('field_session', 'field_session')
      ->sourceMigration('HedleyMigrateSessions');
  }

}
