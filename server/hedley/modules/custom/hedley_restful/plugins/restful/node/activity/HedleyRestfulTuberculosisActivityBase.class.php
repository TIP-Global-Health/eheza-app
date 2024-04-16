<?php

/**
 * @file
 * Contains HedleyRestfulTuberculosisActivityBase.
 */

/**
 * Class HedleyRestfulTuberculosisActivityBase.
 */
abstract class HedleyRestfulTuberculosisActivityBase extends HedleyRestfulActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['tuberculosis_encounter'] = [
      'property' => 'field_tuberculosis_encounter',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    hedley_general_join_field_to_query($query, 'node', 'field_tuberculosis_encounter', FALSE);
    // Get the UUID of the Tuberculosis encounter.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_tuberculosis_encounter.field_tuberculosis_encounter_target_id", 'uuid_tuberculosis_encounter');

    return $query;
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->tuberculosis_encounter = $item->uuid_tuberculosis_encounter;
      unset($item->uuid_tuberculosis_encounter);
    }

    return $items;
  }

}
