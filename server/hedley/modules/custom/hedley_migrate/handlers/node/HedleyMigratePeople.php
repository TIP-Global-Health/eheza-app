<?php

/**
 * @file
 * Contains \HedleyMigratePeople.
 */

/**
 * Class HedleyMigratePeople.
 */
class HedleyMigratePeople extends HedleyMigrateBase {

  protected $entityType = 'node';
  protected $bundle = 'person';

  protected $csvColumns = [
    'id',
    'title',
    'field_gender',
    'field_clinic',
  ];

  protected $simpleMappings = [
    'field_gender',
  ];

  /**
   * HedleyMigrateClinics constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigrateClinics',
    ];

    $this
      ->addFieldMapping('field_clinic', 'field_clinic')
      ->sourceMigration('HedleyMigrateClinics');
  }

}
