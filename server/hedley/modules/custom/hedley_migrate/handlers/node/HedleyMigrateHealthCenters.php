<?php

/**
 * @file
 * Contains \HedleyMigrateHealthCenters.
 */

/**
 * Class HedleyMigrateHealthCenters.
 */
class HedleyMigrateHealthCenters extends HedleyMigrateBase {

  public $entityType = 'node';
  public $bundle = 'health_center';

  protected $csvColumns = [
    'id',
    'title_field',
    'field_catchment_area',
  ];

  /**
   * HedleyMigrateHealthCenters constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigrateCatchmentAreas',
    ];

    $this
      ->addFieldMapping('field_catchment_area', 'field_catchment_area')
      ->sourceMigration('HedleyMigrateCatchmentAreas');
  }

}
