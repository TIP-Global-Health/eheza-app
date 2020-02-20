<?php

/**
 * @file
 * Contains \HedleyRestfulNurses.
 */

/**
 * Class HedleyRestfulNurses.
 */
class HedleyRestfulNurses extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['health_centers'] = [
      'property' => 'field_health_centers',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['villages'] = [
      'property' => 'field_villages',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['pin_code'] = [
      'property' => 'field_pin_code',
    ];

    $public_fields['role'] = [
      'property' => 'field_role',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $field_names = [
      'field_role',
      'field_pin_code',
      'field_health_centers',
      'field_villages',
    ];

    foreach ($field_names as $field_name) {
      hedley_restful_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    $query->addExpression("GROUP_CONCAT(DISTINCT field_role.field_role_value)", 'field_role');

    // Get the UUIDs of the health centers.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_health_centers.field_health_centers_target_id", 'uuids_health_centers');
    $query->addExpression("GROUP_CONCAT(DISTINCT uuids_health_centers.field_uuid_value)", 'uuids_health_centers');

    // Get the UUIDs of the villages.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_villages.field_villages_target_id", 'uuids_villages');
    $query->addExpression("GROUP_CONCAT(DISTINCT uuids_villages.field_uuid_value)", 'uuids_villages');
    $query->groupBy('node.nid');
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->role = explode(',', $item->role);
      $item->health_centers = explode(',', $item->uuids_health_centers);
      unset($item->uuids_health_centers);

      if (empty($item->uuids_villages)) {
        $item->villages = [];
      }
      else {
        $item->villages = explode(',', $item->uuids_villages);
      }
      unset($item->uuids_villages);
    }

    return $items;
  }

}
