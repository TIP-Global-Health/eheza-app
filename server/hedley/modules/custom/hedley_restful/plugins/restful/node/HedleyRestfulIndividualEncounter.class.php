<?php

/**
 * @file
 * Contains \HedleyRestfulIndividualEncounter.
 */

/**
 * Class HedleyRestfulIndividualEncounter.
 */
class HedleyRestfulIndividualEncounter extends HedleyRestfulSyncBase {

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

    $public_fields['individual_participant'] = [
      'property' => 'field_individual_participant',
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
    }

    // The label is decorative only.
    unset($public_fields['label']);

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $field_names = [
      'field_individual_participant',
      'field_deleted',
    ];

    foreach ($field_names as $field_name) {
      hedley_general_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    hedley_general_join_field_to_query($query, 'node', 'field_scheduled_date', FALSE, NULL, NULL, TRUE);

    // Get the UUIDs of the Individual participant.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_individual_participant.field_individual_participant_target_id", 'uuid_individual_participant');

    foreach (array_merge($this->fields, $this->multiFields) as $field_name) {
      hedley_general_join_field_to_query($query, 'node', $field_name, FALSE);
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
      $item->individual_participant = $item->uuid_individual_participant;
      unset($item->uuid_individual_participant);

      $date = [
        'value' => $item->scheduled_date,
        'value2' => $item->field_scheduled_date_field_scheduled_date_value2,
      ];
      $item->scheduled_date = $this->renderDate($date);
      unset($item->field_scheduled_date_field_scheduled_date_value2);

      foreach ($this->multiFields as $field_name) {
        $public_name = str_replace('field_', '', $field_name);
        $item->{$public_name} = explode(',', $item->{$public_name});
      }

      foreach ($this->dateFields as $field_name) {
        $public_name = str_replace('field_', '', $field_name);
        $date = explode(' ', $item->{$public_name});
        $item->{$public_name} = !empty($date[0]) ? $date[0] : NULL;
      }

      unset($item->label);
    }

    return $items;
  }

}
