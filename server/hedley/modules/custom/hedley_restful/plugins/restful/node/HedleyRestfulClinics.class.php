<?php

/**
 * @file
 * Contains \HedleyRestfulClinics.
 */

/**
 * Class HedleyRestfulClinics.
 */
class HedleyRestfulClinics extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['health_center'] = [
      'property' => 'field_health_center',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['village'] = [
      'property' => 'field_village_ref',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['group_type'] = [
      'property' => 'field_group_type',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $field_names = [
      'field_group_type',
      'field_health_center',
      'field_village_ref',
    ];

    foreach ($field_names as $field_name) {
      hedley_restful_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    // Get the UUID of the health center.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_health_center.field_health_center_target_id", 'uuid_health_center');

    // Get the UUID of the village.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_village_ref.field_village_ref_target_id", 'uuid_village');
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->health_center = $item->uuid_health_center;
      unset($item->uuid_health_center);
      $item->village = $item->uuid_village;
      unset($item->uuid_village);
    }

    return $items;
  }

}
