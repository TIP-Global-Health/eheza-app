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

    // Get the UUIDs of the Person and 'Related to' person.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_person.field_person_target_id", 'uuid_person');
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_related_to.field_related_to_target_id", 'uuid_related_to');

  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->person = $item->uuid_person;
      unset($item->uuid_person);
      $item->related_to = $item->uuid_related_to;
      unset($item->uuid_related_to);
    }

    return $items;
  }

}
