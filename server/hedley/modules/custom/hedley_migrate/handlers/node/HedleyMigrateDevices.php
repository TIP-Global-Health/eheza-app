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
    'field_pairing_code',
  ];

  protected $simpleMappings = [
    'field_pairing_code',
  ];

}
