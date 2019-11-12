<?php

/**
 * @file
 * Contains HedleyRestfulActivityBase.
 */

/**
 * Class HedleyRestfulActivityBase.
 */
abstract class HedleyRestfulActivityBase extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['date_measured'] = [
      'property' => 'field_date_measured',
      'process_callbacks' => [
        [$this, 'renderDate'],
      ],
    ];

    $public_fields['nurse'] = [
      'property' => 'field_nurse',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['session'] = [
      'property' => 'field_session',
      'sub_property' => 'field_uuid',
    ];

    // The label is purely decorative.
    unset($public_fields['label']);

    return $public_fields;
  }

  /**
   * Show the date with date only.
   */
  public function renderDate($date) {
    return date("Y-m-d", $date);
  }

  public function viewWithDbSelect(array $node_ids) {
    $query = $this->getQueryForViewWithDbSelect($node_ids);db_select('node', 'node');

    $this->alterQueryForViewWithDbSelect($query);

    $items = $this->executeQueryForViewWithDbSelect($query);
    return $this->postExecuteQueryForViewWithDbSelect($items);
  }


  protected function getQueryForViewWithDbSelect(array $node_ids) {
    $query = db_select('node', 'node');
    $query->fields('node', ['type', 'nid', 'vid', 'created']);
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
    return $items;
  }

}
