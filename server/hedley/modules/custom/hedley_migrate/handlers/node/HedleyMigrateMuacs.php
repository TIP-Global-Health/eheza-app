<?php

/**
 * @file
 * Contains \HedleyMigrateMuacs.
 */

/**
 * Class HedleyMigrateMuacs.
 */
class HedleyMigrateMuacs extends HedleyMigrateMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'muac';

  /**
   * {@inheritdoc}
   */
  protected $csvColumns = [
    'id',
    'field_person',
    'field_date_measured',
    'field_nurse',
    'field_session',
    'field_muac',
  ];

  /**
   * {@inheritdoc}
   */
  protected $simpleMappings = [
    'field_muac',
  ];

}
