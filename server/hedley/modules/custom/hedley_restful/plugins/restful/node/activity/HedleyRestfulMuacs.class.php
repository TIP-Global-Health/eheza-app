<?php

/**
 * @file
 * Contains HedleyRestfulMuacs.
 */

/**
 * Class HedleyRestfulMuacs.
 */
class HedleyRestfulMuacs extends HedleyRestfulChildActivityBase {

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

  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    hedley_restful_join_field_to_query($query, 'node', 'field_muac');
  }

}
