<?php

/**
 * @file
 * Contains \HedleyMigrateAttendances.
 */

/**
 * Class HedleyMigrateAttendances.
 */
class HedleyMigrateAttendances extends HedleyMigrateMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'attendance';

  /**
   * {@inheritdoc}
   */
  protected $csvColumns = [
    'id',
    'field_person',
    'field_date_measured',
    'field_nurse',
    'field_session',
    'field_attended',
  ];

  /**
   * {@inheritdoc}
   */
  protected $simpleMappings = [
    'field_attended',
  ];

}
