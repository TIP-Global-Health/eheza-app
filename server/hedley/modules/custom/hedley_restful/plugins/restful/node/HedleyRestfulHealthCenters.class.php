<?php

/**
 * @file
 * Contains HedleyRestfulHealthCenters.
 */

/**
 * Class HedleyRestfulHealthCenters.
 */
class HedleyRestfulHealthCenters extends HedleyRestfulSyncBase {

  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    hedley_restful_join_field_to_query($query, 'node', 'field_catchment_area');

    // Get the UUID of the health center.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid');
  }

  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$row) {
      $row->uuid = $row->field_uuid;
      unset($row->field_uuid);

      $wrapper = entity_metadata_wrapper('node', $row->field_catchment_area);
      $row->catchment_area = $wrapper->field_uuid->value();
      unset($row->field_catchment_area);
    }
    return $items;
  }

}
