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
    return $query;
  }

  protected function executeQueryForViewWithDbSelect(SelectQuery $query) {
    $result = $query
      ->execute()
      ->fetchAllAssoc('nid');

    return $result;
  }

  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $fields_info = $this->getPublicFields();

    foreach ($items as &$item) {
      foreach ($fields_info as $public_name => $field_info) {
        if (strpos($field_info['property'], 'field_') !== 0) {
          continue;
        }

        $item->{$public_name} = $item->{$field_info['property']};
        unset($item->{$field_info['property']});
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
