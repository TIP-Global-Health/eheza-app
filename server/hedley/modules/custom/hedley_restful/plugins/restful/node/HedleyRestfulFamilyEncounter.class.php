<?php

/**
 * @file
 * Contains \HedleyRestfulFamilyEncounter.
 */

/**
 * Class HedleyRestfulFamilyEncounter.
 */
class HedleyRestfulFamilyEncounter extends HedleyRestfulSyncBase {

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

    $public_fields['family_participant'] = [
      'property' => 'field_family_participant',
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
    $field_names = [
      'field_family_participant',
      'field_deleted',
    ];

    foreach ($field_names as $field_name) {
      hedley_general_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    hedley_general_join_field_to_query($query, 'node', 'field_scheduled_date', FALSE, NULL, NULL, TRUE);

    // Get the UUIDs of the Family participant.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_family_participant.field_family_participant_target_id", 'uuid_family_participant');

    return $query;
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->family_participant = $item->uuid_family_participant;
      unset($item->uuid_family_participant);

      $date = [
        'value' => $item->scheduled_date,
        'value2' => $item->field_scheduled_date_field_scheduled_date_value2,
      ];
      $item->scheduled_date = $this->renderDate($date);
      unset($item->field_scheduled_date_field_scheduled_date_value2);

      unset($item->label);
    }

    return $items;
  }

}
