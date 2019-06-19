<?php

/**
 * @file
 * Contains \HedleyMigrateMissingWeights.
 */

/**
 * Class HedleyMigrateMissingWeights.
 */
class HedleyMigrateMissingWeights extends HedleyMigrateMissing {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'weight';

  /**
   * {@inheritdoc}
   */
  protected $simpleMappings = [
    'field_weight',
  ];

}
