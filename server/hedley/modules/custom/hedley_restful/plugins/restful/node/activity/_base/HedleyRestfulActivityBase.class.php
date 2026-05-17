<?php

/**
 * @file
 * Contains HedleyRestfulActivityBase.
 */

/**
 * Class HedleyRestfulActivityBase.
 */
abstract class HedleyRestfulActivityBase extends HedleyRestfulSyncBase {

  /**
   * A list of fields that are assigned single value.
   *
   * @var array
   */
  protected $fields = [];

  /**
   * A list of fields that are assigned multiple values.
   *
   * @var array
   */
  protected $multiFields = [];

  /**
   * A list of fields that are dates. This is a sub list of $fields.
   *
   * @var array
   */
  protected $dateFields = [];

  /**
   * Fields that are list of dates. This is a sub list of $multiFields.
   *
   * @var array
   */
  protected $multiDateFields = [];

  /**
   * Fields that represent Entity References. This is a sub list of $fields.
   *
   * @var array
   */
  protected $entityFields = [];

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    // The label is purely decorative.
    unset($public_fields['label']);

    $public_fields['date_measured'] = [
      'property' => 'field_date_measured',
      'process_callbacks' => [
        [$this, 'renderDate2'],
      ],
    ];

    $public_fields['nurse'] = [
      'property' => 'field_nurse',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['person'] = [
      'property' => 'field_person',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['deleted'] = [
      'property' => 'field_deleted',
    ];

    foreach (array_merge($this->fields, $this->multiFields) as $field_name) {
      $public_name = str_replace('field_', '', $field_name);

      $public_fields[$public_name] = [
        'property' => $field_name,
      ];

      if (in_array($field_name, $this->dateFields)) {
        $public_fields[$public_name]['process_callbacks'] = [[$this, 'renderDate2']];
      }

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

    $field_names = [
      'field_date_measured',
      'field_nurse',
      'field_person',
      'field_deleted',
    ];

    foreach ($field_names as $field_name) {
      hedley_general_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    // Get the UUID of the Nurse.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_nurse.field_nurse_target_id", 'uuid_nurse');

    // Get the UUID of the Person.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_person.field_person_target_id", 'uuid_person');

    foreach (array_merge($this->fields, $this->multiFields) as $field_name) {
      hedley_general_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    foreach ($this->entityFields as $field_name) {
      $public_name = str_replace('field_', '', $field_name);
      hedley_general_join_field_to_query($query, 'node', 'field_uuid', FALSE, "$field_name.{$field_name}_target_id", "uuid_$public_name");
    }

    foreach ($this->multiFields as $field_name) {
      $query->addExpression("GROUP_CONCAT(DISTINCT $field_name.{$field_name}_value)", $field_name);
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

      $date = explode(' ', $item->date_measured);
      $item->date_measured = !empty($date[0]) ? $date[0] : NULL;
      $item->nurse = $item->uuid_nurse;
      unset($item->uuid_nurse);
      $item->person = $item->uuid_person;
      unset($item->uuid_person);

      foreach ($this->multiFields as $field_name) {
        $public_name = str_replace('field_', '', $field_name);
        $item->{$public_name} = !empty($item->{$public_name}) ? explode(',', $item->{$public_name}) : NULL;
      }

      foreach ($this->entityFields as $field_name) {
        $public_name = str_replace('field_', '', $field_name);
        $uuid_name = "uuid_$public_name";
        $item->{$public_name} = $item->{$uuid_name};
        unset($item->{$uuid_name});
      }

      foreach ($this->dateFields as $field_name) {
        $public_name = str_replace('field_', '', $field_name);
        $date = explode(' ', $item->{$public_name});
        $item->{$public_name} = !empty($date[0]) ? $date[0] : NULL;
      }

      foreach ($this->multiDateFields as $field_name) {
        $public_name = str_replace('field_', '', $field_name);

        if (empty($item->{$public_name})) {
          continue;
        }

        $dates = [];
        foreach ($item->{$public_name} as $raw_date) {
          $date = explode(' ', $raw_date);
          $dates[] = !empty($date[0]) ? $date[0] : NULL;
        }

        $item->{$public_name} = $dates;
      }
    }

    return $items;
  }

}
