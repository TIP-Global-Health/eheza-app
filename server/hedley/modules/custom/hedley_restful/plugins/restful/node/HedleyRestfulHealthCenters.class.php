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

    // Get the UUID of the health center.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_catchment_area.field_catchment_area_target_id");
  }

  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    foreach ($items as &$row) {
      $row->catchment_area = $row->field_catchment_area;
      $row->uuid = $row->field_uuid;

      unset($row->field_catchment_area);
      unset($row->field_uuid);
    }
    return $items;
  }


}
