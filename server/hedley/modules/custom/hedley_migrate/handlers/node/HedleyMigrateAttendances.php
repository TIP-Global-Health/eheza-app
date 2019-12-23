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
  protected $simpleMappings = [
    'field_attended',
  ];

}
