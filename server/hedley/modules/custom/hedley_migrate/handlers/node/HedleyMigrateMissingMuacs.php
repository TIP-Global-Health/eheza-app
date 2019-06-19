<?php

/**
 * @file
 * Contains \HedleyMigrateMissingMuacs.
 */

/**
 * Class HedleyMigrateMissingMuacs.
 */
class HedleyMigrateMissingMuacs extends HedleyMigrateMissing {

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
