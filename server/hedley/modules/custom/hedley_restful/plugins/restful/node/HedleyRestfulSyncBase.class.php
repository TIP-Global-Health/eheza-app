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

  public function viewWithDbSelect(array $node_ids) {
    $query = $this->getQueryForViewWithDbSelect($node_ids);

    $this->alterQueryForViewWithDbSelect($query);

    $items = $this->executeQueryForViewWithDbSelect($query);
    return $this->postExecuteQueryForViewWithDbSelect($items);
  }


  protected function getQueryForViewWithDbSelect(array $node_ids) {
    $query = db_select('node', 'node');
    $query->fields('node', ['type', 'nid', 'vid', 'changed', 'title', 'status']);
    $query->condition('node.nid',$node_ids, 'IN');

    return $query;
  }

  // @todo: Make abstract, so everyone must implement.
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
  }

  protected function executeQueryForViewWithDbSelect(SelectQuery $query) {
    $result = $query
      ->execute()
      ->fetchAllAssoc('nid');

    return $result;
  }

  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    foreach ($items as &$item) {
      $item->id = $item->nid;
      $item->label = $item->title;
      $item->timestamp = $item->changed;

      unset($item->nid);
      unset($item->title);
      unset($item->changed);
    }

    return $items;
  }





}
