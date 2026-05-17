<?php

/**
 * @file
 * Contains HedleyRestfulNCDActivityBase.
 */

/**
 * Class HedleyRestfulNCDActivityBase.
 */
abstract class HedleyRestfulNCDActivityBase extends HedleyRestfulActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['ncd_encounter'] = [
      'property' => 'field_ncd_encounter',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    hedley_general_join_field_to_query($query, 'node', 'field_ncd_encounter', FALSE);
    // Get the UUID of the Prenatal encounter.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_ncd_encounter.field_ncd_encounter_target_id", 'uuid_ncd_encounter');

    return $query;
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->ncd_encounter = $item->uuid_ncd_encounter;
      unset($item->uuid_ncd_encounter);
    }

    return $items;
  }

}
