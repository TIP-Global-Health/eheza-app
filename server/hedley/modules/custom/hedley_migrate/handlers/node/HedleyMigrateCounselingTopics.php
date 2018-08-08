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
    'title_field',
    'field_kinyarwanda_title',
  ];
  protected $simpleMappings = [
    'field_kinyarwanda_title',
  ];

}
