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
    'field_clinics',
    'field_pin_code',
  ];

  /**
   * {@inheritdoc}
   */
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
