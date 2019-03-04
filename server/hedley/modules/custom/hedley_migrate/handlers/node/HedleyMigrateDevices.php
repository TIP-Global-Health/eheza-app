<?php

/**
 * @file
 * Contains \HedleyMigrateDevices.
 */

/**
 * Class HedleyMigrateDevices.
 */
class HedleyMigrateDevices extends HedleyMigrateBase {

  public $entityType = 'node';
  public $bundle = 'device';

  protected $csvColumns = [
    'id',
    'title',
  ];

}
