<?php

/**
 * @file
 * Contains \HedleyMigrateCatchmentAreas.
 */

/**
 * Class HedleyMigrateCatchmentAreas.
 */
class HedleyMigrateCatchmentAreas extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  public $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  public $bundle = 'catchment_area';

  /**
   * {@inheritdoc}
   */
  protected $csvColumns = [
    'id',
    'title',
    'created',
  ];

}
