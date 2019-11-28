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

  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    hedley_restful_join_field_to_query($query, 'node', 'field_height');
    hedley_restful_join_field_to_query($query, 'node', 'field_zscore_age');
  }

  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$row) {
      $row->height = $row->field_height;
      $row->zscore_age = $row->field_zscore_age;

      unset($row->field_height);
      unset($row->field_zscore_age);
    }
    return $items;
  }

}
