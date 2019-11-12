<?php

/**
 * @file
 * Contains HedleyRestfulWeights.
 */

/**
 * Class HedleyRestfulWeights.
 */
class HedleyRestfulWeights extends HedleyRestfulChildActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['weight'] = [
      'property' => 'field_weight',
    ];

    $public_fields['bmi'] = [
      'property' => 'field_bmi',
    ];

    $public_fields['zscore_age'] = [
      'property' => 'field_zscore_age',
    ];

    $public_fields['zscore_length'] = [
      'property' => 'field_zscore_length',
    ];

    $public_fields['zscore_bmi'] = [
      'property' => 'field_zscore_bmi',
    ];

    return $public_fields;
  }

  public function viewWithDbSelect($node_ids) {
    $query = db_select('node', 'node');
    $query->fields('node', ['type', 'nid', 'vid', 'created']);
    $query->condition('node.nid',$node_ids, 'IN');

    $field_names = [
      'field_weight',
      'field_bmi',
      'field_zscore_age',
      'field_zscore_length',
      'field_zscore_bmi',
    ];

    foreach ($field_names as $field_name) {
      hedley_restful_join_field_to_query($query, 'node', $field_name);
    }

    $result = $query
      ->execute()
      ->fetchAllAssoc('node.nid');

    return $result;
  }

}
