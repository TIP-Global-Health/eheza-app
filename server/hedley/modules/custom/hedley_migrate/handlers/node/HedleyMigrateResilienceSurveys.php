<?php

/**
 * @file
 * Contains \HedleyMigrateResilienceSurveys.
 */

/**
 * Class HedleyMigrateNurses.
 */
class HedleyMigrateResilienceSurveys extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  public $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  public $bundle = 'resilience_survey';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'id',
        'title',
        'field_nurse',
        'field_date_measured',
        'field_resilience_survey_type',
        'field_resilience_survey_signs',
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
        'field_resilience_survey_type',
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
        'field_resilience_survey_signs',
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
      'HedleyMigrateNurses',
    ];

    $this
      ->addFieldMapping('field_nurse', 'field_nurse')
      ->sourceMigration('HedleyMigrateNurses');

    $this
      ->addFieldMapping('field_date_measured', 'field_date_measured')
      ->callbacks([$this, 'dateProcess']);
  }

}
