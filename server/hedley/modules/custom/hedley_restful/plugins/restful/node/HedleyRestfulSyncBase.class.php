<?php

/**
 * @file
 * Contains \HedleyRestfulSyncBase.
 */

/**
 * Class HedleyRestfulSyncBase.
 */
class HedleyRestfulSyncBase extends \HedleyRestfulEntityBaseNode {

  /**
   * Overrides \RestfulEntityBaseNode::publicFieldsInfo().
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['type'] = [
      'property' => 'type',
    ];

    $public_fields['status'] = [
      'property' => 'status',
    ];

    $public_fields['vid'] = [
      'property' => 'vid',
    ];

    $public_fields['uuid'] = [
      'property' => 'field_uuid',
    ];

    // We don't use `self`, so we'll omit it for now.
    unset($public_fields['self']);

    return $public_fields;
  }

  /**
   * Generates nodes view for provided nodes.
   *
   * @param array $node_ids
   *   The IDs of nodes to view.
   *
   * @return array
   *   The view of the nodes.
   *
   * @throws \Exception
   */
  public function viewWithDbSelect(array $node_ids) {
    $query = $this->getQueryForViewWithDbSelect($node_ids);

    $this->alterQueryForViewWithDbSelect($query);

    $items = $this->executeQueryForViewWithDbSelect($query);
    return $this->postExecuteQueryForViewWithDbSelect($items);
  }

  /**
   * Generates basic query for view the nodes.
   *
   * @param array $node_ids
   *   The IDs of nodes to view.
   *
   * @return \SelectQuery
   *   The view query
   *
   * @throws \Exception
   */
  protected function getQueryForViewWithDbSelect(array $node_ids) {
    $query = db_select('node', 'node');
    $query->fields('node', ['type', 'nid', 'vid', 'changed', 'title', 'status']);
    $query->condition('node.nid', $node_ids, 'IN');

    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', FALSE);

    return $query;
  }

  /**
   * Alters query for view the nodes.
   *
   * @param \SelectQuery $query
   *   The query objected.
   *
   * @return \SelectQuery
   *   Altered query objected.
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    return $query;
  }

  /**
   * Executes the view query.
   *
   * @param \SelectQuery $query
   *   The query objected.
   *
   * @return array
   *   A list of view node obejcts.
   */
  protected function executeQueryForViewWithDbSelect(SelectQuery $query) {
    $result = $query
      ->execute()
      ->fetchAllAssoc('nid');

    return $result;
  }

  /**
   * Performs post processing for view node objects.
   *
   * @param array $items
   *   View node objects.
   *
   * @return array
   *   Processed view node objects.
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $fields_info = $this->getPublicFields();

    foreach ($items as &$item) {
      foreach ($fields_info as $public_name => $field_info) {
        if (strpos($field_info['property'], 'field_') !== 0) {
          continue;
        }

        if (property_exists($item, $field_info['property'])) {
          $item->{$public_name} = $item->{$field_info['property']};
          unset($item->{$field_info['property']});
        }
      }

      $item->id = $item->nid;
      unset($item->nid);

      $item->label = $item->title;
      unset($item->title);

      $item->timestamp = $item->changed;
      unset($item->changed);
    }

    return $items;
  }

}
