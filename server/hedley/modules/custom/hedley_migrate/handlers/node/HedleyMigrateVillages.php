<?php

/**
 * @file
 * Contains \HedleyMigrateVillages.
 */

/**
 * Class HedleyMigrateVillages.
 */
class HedleyMigrateVillages extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  protected $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'village';

  /**
   * {@inheritdoc}
   */
  protected $csvColumns = [
    'id',
    'field_province',
    'field_district',
    'field_sector',
    'field_cell',
    'field_village',
    'field_health_center',
  ];

  /**
   * {@inheritdoc}
   */
  protected $simpleMappings = [
    'field_province',
    'field_district',
    'field_sector',
    'field_cell',
    'field_village',
  ];

  /**
   * HedleyMigrateVillages constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigrateHealthCenters',
      'HedleyMigrateRelationships',
    ];

    $this
      ->addFieldMapping('field_health_center', 'field_health_center')
      ->sourceMigration('HedleyMigrateHealthCenters');
  }

}
