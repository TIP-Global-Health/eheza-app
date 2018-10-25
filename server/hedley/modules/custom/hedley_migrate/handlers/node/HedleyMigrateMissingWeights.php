<?php

/**
 * @file
 * Contains \HedleyMigrateMissingWeights.
 */

/**
 * Class HedleyMigrateMissingWeights.
 */
class HedleyMigrateMissingWeights extends HedleyMigrateMissing {

  protected $bundle = 'weight';

  protected $simpleMappings = [
    'field_weight',
  ];

}
