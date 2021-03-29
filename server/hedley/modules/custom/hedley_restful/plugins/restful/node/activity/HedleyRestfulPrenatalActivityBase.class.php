<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalActivityBase.
 */

/**
 * Class HedleyRestfulPrenatalActivityBase.
 */
abstract class HedleyRestfulPrenatalActivityBase extends HedleyRestfulActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['prenatal_encounter'] = [
      'property' => 'field_prenatal_encounter',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    hedley_restful_join_field_to_query($query, 'node', 'field_prenatal_encounter', FALSE);
    // Get the UUID of the Prenatal encounter.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_prenatal_encounter.field_prenatal_encounter_target_id", 'uuid_prenatal_encounter');

    return $query;
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->prenatal_encounter = $item->uuid_prenatal_encounter;
      unset($item->uuid_prenatal_encounter);
    }

    return $items;
  }

}
