<?php

/**
 * @file
 * Contains HedleyRestfulWellChildActivityBase.
 */

/**
 * Class HedleyRestfulWellChildActivityBase.
 */
abstract class HedleyRestfulWellChildActivityBase extends HedleyRestfulActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['well_child_encounter'] = [
      'property' => 'field_well_child_encounter',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    hedley_general_join_field_to_query($query, 'node', 'field_well_child_encounter', FALSE);
    // Get the UUID of the Prenatal encounter.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_well_child_encounter.field_well_child_encounter_target_id", 'uuid_well_child_encounter');

    return $query;
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->well_child_encounter = $item->uuid_well_child_encounter;
      unset($item->uuid_well_child_encounter);
    }

    return $items;
  }

}
