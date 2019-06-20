<?php

/**
 * @file
 * Contains \HedleyMigrateMissingHeights.
 */

/**
 * Class HedleyMigrateMissingHeights.
 */
class HedleyMigrateMissingHeights extends HedleyMigrateMissing {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'height';

  /**
   * {@inheritdoc}
   */
  protected $simpleMappings = [
    'field_height',
  ];

}
