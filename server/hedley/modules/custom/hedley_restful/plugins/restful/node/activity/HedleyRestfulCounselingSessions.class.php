<?php

/**
 * @file
 * Contains HedleyRestfulCounselingSessions.
 */

/**
 * Class HedleyRestfulCounselingSessions.
 */
class HedleyRestfulCounselingSessions extends HedleyRestfulActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['topics'] = [
      'property' => 'field_topics',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['timing'] = [
      'property' => 'field_timing',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    $field_names = [
      'field_timing',
      'field_topics',
    ];

    foreach ($field_names as $field_name) {
      hedley_restful_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    // Get the UUID of the health center.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_topics.field_topics_target_id", 'uuids_topics');
    $query->addExpression("GROUP_CONCAT(DISTINCT uuids_topics.field_uuid_value)", 'uuids_topics');
    $query->groupBy('node.nid');
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->topics = explode(',', $item->uuids_topics);
      unset($item->uuids_topics);
    }

    return $items;
  }

}
