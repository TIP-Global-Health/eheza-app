<?php

/**
 * @file
 * Contains HedleyRestfulRelationships.
 */

/**
 * Class HedleyRestfulRelationships.
 */
class HedleyRestfulRelationships extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $standard_fields_names = [
      'field_related_by',
    ];

    foreach ($standard_fields_names as $field_name) {
      $public_name = str_replace('field_', '', $field_name);
      $public_fields[$public_name] = [
        'property' => $field_name,
      ];
    }

    $public_fields['person'] = [
      'property' => 'field_person',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['related_to'] = [
      'property' => 'field_related_to',
      'sub_property' => 'field_uuid',
    ];

    // The label is decorative only.
    unset($public_fields['label']);

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $field_names = [
      'field_related_by',
      'field_person',
      'field_related_to',
    ];

    foreach ($field_names as $field_name) {
      hedley_restful_join_field_to_query($query, 'node', $field_name, FALSE);
    }
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->person = hedley_restful_nid_to_uuid($item->person);
      $item->related_to = hedley_restful_nid_to_uuid($item->related_to);
    }

    return $items;
  }

}
