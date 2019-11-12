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

  public function alterQueryForViewWithDbSelect(SelectQuery $query) {
    hedley_restful_join_field_to_query($query, 'node', 'field_height');
    hedley_restful_join_field_to_query($query, 'node', 'field_zscore_age');
  }

}
