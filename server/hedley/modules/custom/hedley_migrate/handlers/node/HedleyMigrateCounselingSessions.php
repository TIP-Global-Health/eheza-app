<?php

/**
 * @file
 * Contains \HedleyMigrateCounselingSessions.
 */

/**
 * Class HedleyMigrateCounselingSessions.
 */
class HedleyMigrateCounselingSessions extends HedleyMigrateBase {

  protected $entityType = 'node';
  protected $bundle = 'counseling';
  protected $csvColumns = [
    'id',
    'topics',
  ];

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
      ->addFieldMapping('field_topics', 'topics')
      ->separator('|')
      ->sourceMigration('HedleyMigrateCounselingTopics');
  }

  /**
   * Add reference values to the entity.
   *
   * @param object $entity
   *   The entity.
   */
  public function prepare($entity) {
    // Get one random child to reference in this counseling session.
    // @todo: Convert the child & session migration to a csv migration.
    $child_query = new EntityFieldQuery();
    $child_result = $child_query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'child')
      ->propertyCondition('status', NODE_PUBLISHED)
      ->range(0, 1)
      ->addTag('random')
      ->execute();

    if (!empty($child_result['node'])) {
      $entity->field_child[LANGUAGE_NONE][0]['target_id'] = key($child_result['node']);
    }

    $today = date('Y-m-d');
    $session_query = new EntityFieldQuery();
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
