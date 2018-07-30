<?php

/**
 * @file
 * Contains \HedleyMigrateClinics.
 */

/**
 * Class HedleyMigrateClinics.
 */
class HedleyMigrateClinics extends HedleyMigrateBase {

  protected $entityType = 'node';
  protected $bundle = 'clinic';

  protected $csvColumns = [
    'id',
    'title',
  ];
}
