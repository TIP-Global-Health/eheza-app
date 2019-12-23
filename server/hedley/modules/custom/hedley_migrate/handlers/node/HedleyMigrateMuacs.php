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
  protected $simpleMappings = [
    'field_muac',
  ];

}
