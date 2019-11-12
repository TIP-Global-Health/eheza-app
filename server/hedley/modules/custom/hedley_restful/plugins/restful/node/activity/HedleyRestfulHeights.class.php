<?php

/**
 * @file
 * Contains HedleyRestfulHeights.
 */

/**
 * Class HedleyRestfulHeights.
 */
class HedleyRestfulHeights extends HedleyRestfulChildActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['height'] = [
      'property' => 'field_height',
    ];

    $public_fields['zscore_age'] = [
      'property' => 'field_zscore_age',
    ];

    return $public_fields;
  }

  public function viewWithDbSelect($node_ids) {
    $query = db_select('node', 'n');
    $query->fields('n', ['type', 'nid', 'vid', 'created']);
    $query->condition('n.nid',$node_ids, 'IN');

    hedley_stats_join_field_to_query($query, 'node', 'field_height');
    hedley_stats_join_field_to_query($query, 'node', 'field_zscore_age');

    $result = $query
      ->execute()
      ->fetchAllAssoc('n.nid');

    return $result;
  }

}
