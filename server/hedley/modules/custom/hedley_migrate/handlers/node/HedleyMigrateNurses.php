<?php

/**
 * @file
 * Contains \HedleyMigrateNurses.
 */

/**
 * Class HedleyMigrateNurses.
 */
class HedleyMigrateNurses extends HedleyMigrateBase {

  public $entityType = 'node';
  public $bundle = 'nurse';

  protected $csvColumns = [
    'id',
    'title',
    'field_role',
    'field_clinics',
  ];

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
      ->addFieldMapping('field_clinics', 'field_clinics')
      ->separator('|')
      ->sourceMigration('HedleyMigrateClinics');
  }

}
