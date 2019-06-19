<?php

/**
 * @file
 * Contains \HedleyMigrateCounselingTopics.
 */

/**
 * Class HedleyMigrateCounselingTopics.
 */
class HedleyMigrateCounselingTopics extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  protected $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'counseling_topic';

  /**
   * {@inheritdoc}
   */
  protected $csvColumns = [
    'id',
    'title_field',
    'field_kinyarwanda_title',
  ];

  /**
   * {@inheritdoc}
   */
  protected $simpleMappings = [
    'field_kinyarwanda_title',
  ];

}
