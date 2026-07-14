<?php

/**
 * @file
 * Contains \HedleyRestfulEducationSession.
 */

/**
 * Class HedleyRestfulEducationSession.
 */
class HedleyRestfulEducationSession extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['scheduled_date'] = [
      'property' => 'field_scheduled_date',
      'process_callbacks' => [
        [$this, 'renderDate'],
      ],
    ];

    $public_fields['nurse'] = [
      'property' => 'field_nurse',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['village_ref'] = [
      'property' => 'field_village_ref',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['education_topics'] = [
      'property' => 'field_education_topics',
    ];

    $public_fields['participating_patients'] = [
      'property' => 'field_participating_patients',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['deleted'] = [
      'property' => 'field_deleted',
    ];

    // The label is decorative only.
    unset($public_fields['label']);

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    hedley_general_join_field_to_query($query, 'node', 'field_scheduled_date', FALSE, NULL, NULL, TRUE);

    $field_names = [
      'field_nurse',
      'field_village_ref',
      'field_education_topics',
      'field_participating_patients',
      'field_deleted',
    ];
    foreach ($field_names as $field_name) {
      hedley_general_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    // Get the UUID of the Nurse.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_nurse.field_nurse_target_id", 'uuid_nurse');

    // Get the UUID of the Village.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_village_ref.field_village_ref_target_id", 'uuid_village_ref');

    // Get multiple values of education topics.
    $query->addExpression("GROUP_CONCAT(DISTINCT field_education_topics.field_education_topics_value)", 'field_education_topics');

    // Get the UUIDs of the participating patients.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_participating_patients.field_participating_patients_target_id", 'uuids_participating_patients');
    $query->addExpression("GROUP_CONCAT(DISTINCT uuids_participating_patients.field_uuid_value)", 'uuids_participating_patients');

    $query->groupBy('node.nid');

    return $query;
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $date = [
        'value' => $item->scheduled_date,
        'value2' => $item->field_scheduled_date_field_scheduled_date_value2,
      ];
      $item->scheduled_date = $this->renderDate($date);
      unset($item->field_scheduled_date_field_scheduled_date_value2);

      $item->nurse = $item->uuid_nurse;
      unset($item->uuid_nurse);

      $item->village_ref = $item->uuid_village_ref;
      unset($item->uuid_village_ref);

      // A session is created empty - the client POSTs it the moment the nurse
      // begins the encounter - so both of these are routinely NULL here, and an
      // unguarded explode() would turn that into a single empty string. The
      // empty topic is dropped by the client's decoder, but an empty
      // participant UUID is accepted, and lands in the session's participant
      // set. NULL instead: the decoder's `optional` falls back to an empty set.
      $item->education_topics = !empty($item->education_topics) ? explode(',', $item->education_topics) : NULL;

      $item->participating_patients = !empty($item->uuids_participating_patients) ? explode(',', $item->uuids_participating_patients) : NULL;
      unset($item->uuids_participating_patients);

      unset($item->label);
    }

    return $items;
  }

}
