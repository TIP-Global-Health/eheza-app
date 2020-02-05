<?php

/**
 * @file
 * Contains HedleyRestfulHealthCenters.
 */

/**
 * Class HedleyRestfulHealthCenters.
 */
class HedleyRestfulHealthCenters extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['catchment_area'] = [
      'property' => 'field_catchment_area',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    hedley_restful_join_field_to_query($query, 'node', 'field_catchment_area');

    // Get the UUID of the catchment area.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_catchment_area.field_catchment_area_target_id", 'uuid_catchment_area');
  }

  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->catchment_area = $item->uuid_catchment_area;
      unset($item->uuid_catchment_area);
    }
    return $items;
  }

}
