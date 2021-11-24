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
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'id',
        'title_field',
        'field_kinyarwanda_title',
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
        'field_kinyarwanda_title',
      ]
    );
  }

}
