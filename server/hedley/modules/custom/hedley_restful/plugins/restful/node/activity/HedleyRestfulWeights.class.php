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

  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
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
  }

  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    $fields_info = $this->getPublicFields();

    foreach ($items as &$row) {
      foreach ($fields_info as $public_name => $field_info) {
        if (strpos($field_info['property'], 'field_') !== 0) {
          continue;
        }

        if (!isset($row->{$field_info['property']})) {
          continue;
        }

        $row->{$public_name} = $row->{$field_info['property']};
        unset($row->{$field_info['property']});
      }
    }
    return $items;
  }

}
