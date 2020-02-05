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
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid');
  }

  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->uuid = $item->field_uuid;
      unset($item->field_uuid);

      $item->catchment_area = hedley_restful_nid_to_uuid($item->field_catchment_area);
      unset($item->field_catchment_area);
    }
    return $items;
  }

}
