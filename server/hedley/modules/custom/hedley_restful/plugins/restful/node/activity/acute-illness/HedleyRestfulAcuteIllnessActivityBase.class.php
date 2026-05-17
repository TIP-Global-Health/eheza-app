<?php

/**
 * @file
 * Contains HedleyRestfulAcuteIllnessActivityBase.
 */

/**
 * Class HedleyRestfulAcuteIllnessActivityBase.
 */
abstract class HedleyRestfulAcuteIllnessActivityBase extends HedleyRestfulActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['acute_illness_encounter'] = [
      'property' => 'field_acute_illness_encounter',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    hedley_general_join_field_to_query($query, 'node', 'field_acute_illness_encounter', FALSE);
    // Get the UUID of the Prenatal encounter.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_acute_illness_encounter.field_acute_illness_encounter_target_id", 'uuid_acute_illness_encounter');

    return $query;
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->acute_illness_encounter = $item->uuid_acute_illness_encounter;
      unset($item->uuid_acute_illness_encounter);
    }

    return $items;
  }

}
