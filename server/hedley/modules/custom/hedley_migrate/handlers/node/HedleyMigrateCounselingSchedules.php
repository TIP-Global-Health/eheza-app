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
  protected $csvColumns = [
    'id',
    'field_timing',
    'field_topics',
  ];

  /**
   * {@inheritdoc}
   */
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
