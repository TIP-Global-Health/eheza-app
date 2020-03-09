<?php

/**
 * @file
 * Contains HedleyRestfulChildFBFs.
 */

/**
 * Class HedleyRestfulChildFBFs.
 */
class HedleyRestfulChildFBFs extends HedleyRestfulActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['distributed_amount'] = [
      'property' => 'field_distributed_amount',
    ];

    $public_fields['distribution_notice'] = [
      'property' => 'field_distribution_notice',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    hedley_restful_join_field_to_query($query, 'node', 'field_distributed_amount', FALSE);
    hedley_restful_join_field_to_query($query, 'node', 'field_distribution_notice', FALSE);
  }

}
