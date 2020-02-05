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
    ];

    foreach ($field_names as $field_name) {
      hedley_restful_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    // Get the UUID of the health center.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_health_centers.field_health_centers_target_id");

    $query->addExpression("GROUP_CONCAT(DISTINCT field_role.field_role_value)", 'field_role');
    $query->addExpression("GROUP_CONCAT(DISTINCT field_health_centers.field_health_centers_target_id)", 'field_health_centers');
    $query->groupBy('node.nid');
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->role = explode(',', $item->role);

      $health_centers = explode(',', $item->health_centers);
      foreach ($health_centers as &$health_center) {
        $health_center = hedley_restful_nid_to_uuid($health_center);
      }

      $item->health_centers = $health_centers;
    }

    return $items;
  }

}
