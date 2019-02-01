<?php

/**
 * @file
 * Contains \HedleyMigrateCatchmentAreas.
 */

/**
 * Class HedleyMigrateCatchmentAreas.
 */
class HedleyMigrateCatchmentAreas extends HedleyMigrateBase {

  public $entityType = 'node';
  public $bundle = 'catchment_area';

  protected $csvColumns = [
    'id',
    'title',
  ];

}
