<?php

/**
 * @file
 * Contains \HedleyMigrateMeasurementBase.
 */

/**
 * Class HedleyMigrateMeasurementBase.
 */
abstract class HedleyMigrateMeasurementBase extends HedleyMigrateBase {

  /**
   * The entity type.
   *
   * @var string
   */
  protected $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'id',
        'field_person',
        'field_date_measured',
        'field_nurse',
        'created',
      ]
    );
  }

  /**
   * HedleyMigrateMeasurementBase constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigratePeople',
      'HedleyMigrateNurses',
      'HedleyMigrateHealthCenters',
    ];

    $this
      ->addFieldMapping('field_date_measured', 'field_date_measured')
      ->callbacks([$this, 'dateProcess']);

    $this
      ->addFieldMapping('field_person', 'field_person')
      ->sourceMigration('HedleyMigratePeople');

    $this
      ->addFieldMapping('field_nurse', 'field_nurse')
      ->sourceMigration('HedleyMigrateNurses');
  }

}
