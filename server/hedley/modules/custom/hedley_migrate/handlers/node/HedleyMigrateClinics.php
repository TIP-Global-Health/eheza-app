<?php

/**
 * @file
 * Contains \HedleyMigrateClinics.
 */

/**
 * Class HedleyMigrateClinics.
 */
class HedleyMigrateClinics extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  protected $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'clinic';

  /**
   * {@inheritdoc}
   */
  protected $csvColumns = [
    'id',
    'title',
    'field_group_type',
    'field_health_center',
    'created',
  ];

  /**
   * {@inheritdoc}
   */
  protected $simpleMappings = [
    'field_group_type',
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
