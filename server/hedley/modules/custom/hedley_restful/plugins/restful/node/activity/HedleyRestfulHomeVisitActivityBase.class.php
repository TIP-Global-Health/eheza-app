<?php

/**
 * @file
 * Contains HedleyRestfulHomeVisitActivityBase.
 */

/**
 * Class HedleyRestfulHomeVisitActivityBase.
 */
abstract class HedleyRestfulHomeVisitActivityBase extends HedleyRestfulActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['home_visit_encounter'] = [
      'property' => 'field_home_visit_encounter',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    hedley_restful_join_field_to_query($query, 'node', 'field_home_visit_encounter', FALSE);
    // Get the UUID of the Prenatal encounter.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_home_visit_encounter.field_home_visit_encounter_target_id", 'uuid_home_visit_encounter');

    return $query;
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->home_visit_encounter = $item->uuid_home_visit_encounter;
      unset($item->uuid_home_visit_encounter);
    }

    return $items;
  }

}
