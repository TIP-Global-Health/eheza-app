<?php

/**
 * @file
 * Contains HedleyRestfulVillages.
 */

/**
 * Class HedleyRestfulVillages.
 */
class HedleyRestfulVillages extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $standard_fields_names = [
      'field_cell',
      'field_district',
      'field_province',
      'field_sector',
      'field_village',
    ];

    foreach ($standard_fields_names as $field_name) {
      $public_name = str_replace('field_', '', $field_name);
      $public_fields[$public_name] = [
        'property' => $field_name,
      ];
    }

    $public_fields['health_center'] = [
      'property' => 'field_health_center',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $field_names = [
      'field_cell',
      'field_district',
      'field_province',
      'field_sector',
      'field_village',
      'field_health_center',
    ];

    foreach ($field_names as $field_name) {
      hedley_restful_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    // Get the UUID of the health center.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_health_center.field_health_center_target_id", 'uuid_health_center');
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->health_center = $item->uuid_health_center;
      unset($item->uuid_health_center);
    }

    return $items;
  }

}
