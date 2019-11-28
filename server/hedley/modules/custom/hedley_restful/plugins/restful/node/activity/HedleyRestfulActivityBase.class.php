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

    $public_fields['session'] = [
      'property' => 'field_session',
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


  // @todo: Get public fields here as-well.
  protected function getQueryForViewWithDbSelect(array $node_ids) {
    $query = db_select('node', 'node');
    $query->fields('node', ['type', 'nid', 'vid', 'created']);
    $query->condition('node.nid',$node_ids, 'IN');

    $field_names = [
      'field_date_measured',
      'field_nurse',
      'field_session',
      'field_person',
    ];

    foreach ($field_names as $field_name) {
      hedley_restful_join_field_to_query($query, 'node', $field_name);
    }

    // Get the UUID of the Nurse.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_nurse.field_nurse_target_id", 'field_uuid_of_nurse');

    // Get the UUID of the Session.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_session.field_session_target_id", 'field_uuid_of_session');

    // Get the UUID of the Person.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_person.field_person_target_id", 'field_uuid_of_person');

    return $query;
  }

  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    foreach ($items as &$item) {
      $date = explode(' ', $item->field_date_measured);
      $item->date_measured = !empty($date[0]) ? $date[0] : '1970-01-01';
      $item->nurse = $item->field_uuid_of_nurse;
      $item->session = $item->field_uuid_of_session;
      $item->person = $item->field_uuid_of_person;

      unset($item->field_date_measured);
      unset($item->field_uuid_of_nurse);
      unset($item->field_uuid_of_session);
      unset($item->field_uuid_of_person);
    }

    return $items;
  }


}
