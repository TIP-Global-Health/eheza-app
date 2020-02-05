<?php

/**
 * @file
 * Contains HedleyRestfulCounselingSchedule.
 */

/**
 * Class HedleyRestfulCounselingSchedule.
 */
class HedleyRestfulCounselingSchedule extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['timing'] = [
      'property' => 'field_timing',
    ];

    $public_fields['topics'] = [
      'property' => 'field_topics',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $field_names = [
      'field_timing',
      'field_topics',
    ];

    foreach ($field_names as $field_name) {
      hedley_restful_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    // Get the UUID of the health center.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_topics.field_topics_target_id");

    $query->addExpression("GROUP_CONCAT(DISTINCT field_topics.field_topics_target_id)", 'field_topics');
    $query->groupBy('node.nid');
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->topics = hedley_restful_nid_to_uuid($item->topics);
    }

    return $items;
  }

}
