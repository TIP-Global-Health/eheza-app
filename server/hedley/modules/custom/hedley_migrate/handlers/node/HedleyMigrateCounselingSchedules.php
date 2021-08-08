<?php

/**
 * @file
 * Contains \HedleyMigrateCounselingSchedules.
 */

/**
 * Class .
 */
class HedleyMigrateCounselingSchedules extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  protected $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'counseling_schedule';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'id',
        'field_timing',
        'field_topics',
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
        'field_timing',
      ]
    );
  }

  /**
   * HedleyMigrateCounselingSchedules constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);
    $this->dependencies = [
      'HedleyMigrateCounselingTopics',
    ];

    $this
      ->addFieldMapping('field_topics', 'field_topics')
      ->separator('|')
      ->sourceMigration('HedleyMigrateCounselingTopics');
  }

}
