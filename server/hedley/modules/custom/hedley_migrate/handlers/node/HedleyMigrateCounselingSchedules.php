<?php

/**
 * @file
 * Contains \HedleyMigrateCounselingSchedules.
 */

/**
 * Class .
 */
class HedleyMigrateCounselingSchedules extends HedleyMigrateBase {

  protected $entityType = 'node';
  protected $bundle = 'counseling_schedule';
  protected $csvColumns = [
    'id',
    'field_timing',
    'field_topics',
  ];
  protected $simpleMappings = [
    'field_timing',
  ];

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
