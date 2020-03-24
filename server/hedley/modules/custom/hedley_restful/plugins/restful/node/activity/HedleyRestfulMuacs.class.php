<?php

/**
 * @file
 * Contains HedleyRestfulMuacs.
 */

/**
 * Class HedleyRestfulMuacs.
 */
class HedleyRestfulMuacs extends HedleyRestfulActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['muac'] = [
      'property' => 'field_muac',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    hedley_restful_join_field_to_query($query, 'node', 'field_muac', FALSE);
  }

}
