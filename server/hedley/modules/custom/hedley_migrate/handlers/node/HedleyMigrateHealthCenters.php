<?php

/**
 * @file
 * Contains \HedleyMigrateHealthCenters.
 */

/**
 * Class HedleyMigrateHealthCenters.
 */
class HedleyMigrateHealthCenters extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  public $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  public $bundle = 'health_center';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'id',
        'title_field',
        'field_catchment_area',
        'created',
      ]
    );
  }

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
