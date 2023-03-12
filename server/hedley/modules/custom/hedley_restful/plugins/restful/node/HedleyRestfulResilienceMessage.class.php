<?php

/**
 * @file
 * Contains HedleyRestfulResilienceMessage.
 */

/**
 * Class HedleyRestfulResilienceMessage.
 */
class HedleyRestfulResilienceMessage extends HedleyRestfulSyncBase {

  /**
   * A list of fields that are assigned single value.
   *
   * @var array
   */
  protected $fields = [
    'field_nurse',
    'field_resilience_category',
    'field_resilience_order',
    'field_display_day',
    'field_time_read',
    'field_next_reminder',
    'field_favorite_message',
  ];

  /**
   * Fields that represent Entity References. This is a sub list of $fields.
   *
   * @var array
   */
  protected $entityFields = [
    'field_nurse',
  ];

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    // The label is purely decorative.
    unset($public_fields['label']);

    foreach ($this->fields as $field_name) {
      $public_name = str_replace('field_', '', $field_name);

      $public_fields[$public_name] = [
        'property' => $field_name,
      ];

      if (in_array($field_name, $this->entityFields)) {
        $public_fields[$public_name]['sub_property'] = 'field_uuid';
      }
    }

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    foreach ($this->fields as $field_name) {
      hedley_general_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    foreach ($this->entityFields as $field_name) {
      $public_name = str_replace('field_', '', $field_name);
      hedley_general_join_field_to_query($query, 'node', 'field_uuid', FALSE, "$field_name.{$field_name}_target_id", "uuid_$public_name");
    }

    $query->groupBy('node.nid');

    return $query;
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      unset($item->label);

      foreach ($this->entityFields as $field_name) {
        $public_name = str_replace('field_', '', $field_name);
        $uuid_name = "uuid_$public_name";
        $item->{$public_name} = $item->{$uuid_name};
        unset($item->{$uuid_name});
      }

      $item->favorite_message = (bool) $item->favorite_message;
    }

    return $items;
  }

}
