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
    'field_pin_code',
  ];

  protected $simpleMultipleMappings = [
    'field_role',
    'field_pin_code',
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
