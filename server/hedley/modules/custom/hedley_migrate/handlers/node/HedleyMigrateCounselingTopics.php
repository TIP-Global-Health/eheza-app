<?php

/**
 * @file
 * Contains \HedleyMigrateCounselingTopics.
 */

/**
 * Class HedleyMigrateCounselingTopics.
 */
class HedleyMigrateCounselingTopics extends HedleyMigrateBase {

  protected $entityType = 'node';
  protected $bundle = 'counseling_topic';
  protected $csvColumns = [
    'id',
    'field_english_title',
    'field_kinyarwanda_title',
    'timing',
  ];
  protected $simpleMappings = [
    'field_english_title',
    'field_kinyarwanda_title',
  ];

  /**
   * HedleyMigrateCounselingTopics constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);
    $this->dependencies = [];

    $this
      ->addFieldMapping('field_timing', 'timing')
      ->separator('|');
  }

}
