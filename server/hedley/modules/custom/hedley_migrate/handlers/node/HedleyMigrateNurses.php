<?php

/**
 * @file
 * Contains \HedleyMigrateNurses.
 */

/**
 * Class HedleyMigrateNurses.
 */
class HedleyMigrateNurses extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  public $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  public $bundle = 'nurse';

  /**
   * {@inheritdoc}
   */
  protected $csvColumns = [
    'id',
    'title',
    'field_role',
    'field_health_centers',
    'field_pin_code',
  ];

  /**
   * {@inheritdoc}
   */
  protected $simpleMappings = [
    'field_pin_code',
  ];

  /**
   * {@inheritdoc}
   */
  protected $simpleMultipleMappings = [
    'field_role',
  ];

  /**
   * HedleyMigrateNurses constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigrateClinics',
    ];

    $this
      ->addFieldMapping('field_health_centers', 'field_health_centers')
      ->separator('|')
      ->sourceMigration('HedleyMigrateHealthCenters');
  }

}
