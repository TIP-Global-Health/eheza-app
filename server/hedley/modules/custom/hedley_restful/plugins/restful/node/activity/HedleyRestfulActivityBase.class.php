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
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['date_measured'] = [
      'property' => 'field_date_measured',
      'process_callbacks' => [
        [$this, 'renderDate'],
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

    // The label is purely decorative.
    unset($public_fields['label']);

    return $public_fields;
  }

  /**
   * Show the date with date only.
   */
  public function renderDate($date) {
    return date("Y-m-d", $date);
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
    ];

    foreach ($field_names as $field_name) {
      hedley_restful_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    // Get the UUID of the Nurse.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_nurse.field_nurse_target_id", 'uuid_nurse');

    // Get the UUID of the Person.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_person.field_person_target_id", 'uuid_person');

    return $query;
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $date = explode(' ', $item->date_measured);
      $item->date_measured = !empty($date[0]) ? $date[0] : NULL;
      $item->nurse = $item->uuid_nurse;
      unset($item->uuid_nurse);
      $item->person = $item->uuid_person;
      unset($item->uuid_person);

      unset($item->label);
    }

    return $items;
  }

}
