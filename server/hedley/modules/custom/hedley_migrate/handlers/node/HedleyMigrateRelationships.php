<?php

/**
 * @file
 * Contains \HedleyMigrateRelationships.
 */

/**
 * Class HedleyMigrateRelationships.
 */
class HedleyMigrateRelationships extends HedleyMigrateBase {

  protected $entityType = 'node';
  protected $bundle = 'relationship';

  protected $csvColumns = [
    'id',
    'field_person',
    'field_related_by',
    'field_related_to',
  ];

  protected $simpleMappings = [
    'field_related_by',
  ];

  /**
   * HedleyMigrateRelationships constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigratePeople',
    ];

    $this
      ->addFieldMapping('field_person', 'field_person')
      ->sourceMigration('HedleyMigratePeople');

    $this
      ->addFieldMapping('field_related_to', 'field_related_to')
      ->sourceMigration('HedleyMigratePeople');
  }

}
