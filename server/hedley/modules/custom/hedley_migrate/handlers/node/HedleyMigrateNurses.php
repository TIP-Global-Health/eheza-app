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
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'id',
        'title',
        'field_role',
        'field_health_centers',
        'field_villages',
        'field_pin_code',
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
        'field_pin_code',
      ]
    );
  }

  /**
   * {@inheritdoc}
   */
  protected function simpleMultipleMappings() {
    $mappings = parent::simpleMultipleMappings();

    return array_merge(
      $mappings, [
        'field_role',
      ]
    );
  }

  /**
   * HedleyMigrateNurses constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigrateClinics',
      'HedleyMigrateHealthCenters',
      'HedleyMigrateVillages',
    ];

    $this
      ->addFieldMapping('field_health_centers', 'field_health_centers')
      ->separator('|')
      ->sourceMigration('HedleyMigrateHealthCenters');

    $this
      ->addFieldMapping('field_villages', 'field_villages')
      ->separator('|')
      ->sourceMigration('HedleyMigrateVillages');
  }

}
