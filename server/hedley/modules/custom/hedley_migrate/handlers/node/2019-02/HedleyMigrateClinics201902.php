<?php

/**
 * @file
 * Contains \HedleyMigrateClinics201902.
 */

/**
 * Class HedleyMigrateClinics201902.
 */
class HedleyMigrateClinics201902 extends HedleyMigrateBase {

  public $entityType = 'node';
  public $bundle = 'clinic';
  public $csvPrefix = '2019-02/';

  protected $csvColumns = [
    'id',
    'title',
    'field_health_center',
  ];

  /**
   * HedleyMigrateClinics constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigrateHealthCenters',
    ];

    $this
      ->addFieldMapping('field_health_center', 'field_health_center')
      ->sourceMigration('HedleyMigrateHealthCenters');
  }

}
