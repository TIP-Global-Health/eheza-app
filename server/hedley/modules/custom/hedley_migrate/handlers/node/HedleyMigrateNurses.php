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
        'field_pin_code',
        'created',
        // New.
        'field_resilience_program',
        'field_resilience_start_date',
        'field_resilience_role',
        'field_birth_date',
        'field_gender',
        'field_education_level',
        'field_ubudehe',
        'field_marital_status',
        'field_next_reminder',
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
        'field_resilience_program',
        'field_resilience_role',
        'field_gender',
        'field_education_level',
        'field_ubudehe',
        'field_marital_status',
        'field_next_reminder',
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

    $this
      ->addFieldMapping('field_resilience_start_date', 'field_resilience_start_date')
      ->callbacks([$this, 'dateProcess']);

    $this
      ->addFieldMapping('field_birth_date', 'field_birth_date')
      ->callbacks([$this, 'dateProcess']);
  }

}
