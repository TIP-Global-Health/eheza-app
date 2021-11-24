<?php

/**
 * @file
 * Contains \HedleyMigrateRelationships.
 */

/**
 * Class HedleyMigrateRelationships.
 */
class HedleyMigrateRelationships extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  protected $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'relationship';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'id',
        'field_person',
        'field_related_by',
        'field_related_to',
        'created',
      ]
    );
  }

  /**
   * {@inheritdoc}
   */
  protected function simpleMappings() {
    $mappings = parent::simpleMappings();

    return array_merge(
      $mappings, [
        'field_related_by',
      ]
    );
  }

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
