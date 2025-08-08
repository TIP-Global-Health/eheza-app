<?php

/**
 * @file
 * Contains \HedleyMigrateCounselingSessions.
 */

/**
 * Class HedleyMigrateCounselingSessions.
 */
class HedleyMigrateCounselingSessions extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  protected $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'counseling_session';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'id',
        'field_date_measured',
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
        'field_date_measured',
        'field_timing',
      ]
    );
  }

  /**
   * HedleyMigrateCounselingSessions constructor.
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

  /**
   * Temporary; Add random reference values to the entity.
   *
   * @param object $entity
   *   The entity.
   *
   * @todo: Remove when converting randomly generated data to CSV data.
   */
  public function prepare($entity) {
    // Get one random child to reference in this counseling session.
    $child_query = hedley_general_create_entity_field_query_excluding_deleted();
    $child_result = $child_query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'person')
      ->propertyCondition('status', NODE_PUBLISHED)
      ->range(0, 1)
      ->addTag('random')
      ->execute();

    if (!empty($child_result['node'])) {
      $entity->field_person[LANGUAGE_NONE][0]['target_id'] = key($child_result['node']);
    }

    // Get one random session to reference in this counseling session.
    $today = date('Y-m-d');
    $session_query = hedley_general_create_entity_field_query_excluding_deleted();
    $session_result = $session_query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'session')
      ->propertyCondition('status', NODE_PUBLISHED)
      ->fieldCondition('field_scheduled_date', 'value', $today)
      ->range(0, 1)
      ->addTag('random')
      ->execute();

    if (!empty($session_result['node'])) {
      $entity->field_session[LANGUAGE_NONE][0]['target_id'] = key($session_result['node']);
    }
  }

}
