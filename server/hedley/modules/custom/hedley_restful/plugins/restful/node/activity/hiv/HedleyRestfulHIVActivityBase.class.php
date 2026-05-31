<?php

/**
 * @file
 * Contains HedleyRestfulHIVActivityBase.
 */

/**
 * Class HedleyRestfulHIVActivityBase.
 */
abstract class HedleyRestfulHIVActivityBase extends HedleyRestfulActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['hiv_encounter'] = [
      'property' => 'field_hiv_encounter',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    hedley_general_join_field_to_query($query, 'node', 'field_hiv_encounter', FALSE);
    // Get the UUID of the HIV encounter.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_hiv_encounter.field_hiv_encounter_target_id", 'uuid_hiv_encounter');

    return $query;
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->hiv_encounter = $item->uuid_hiv_encounter;
      unset($item->uuid_hiv_encounter);
    }

    return $items;
  }

}
