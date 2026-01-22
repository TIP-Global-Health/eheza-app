<?php

/**
 * @file
 * Contains HedleyRestfulStockUpdate.
 */

/**
 * Class HedleyRestfulStockUpdate.
 */
class HedleyRestfulStockUpdate extends HedleyRestfulSyncBase {

  /**
   * A list of fields that are dates. This is a sub list of $fields.
   *
   * @var array
   */
  protected $dateFields = [
    'field_date_measured',
    'field_execution_date',
    'field_expiration_date',
  ];

  /**
   * A list of fields that are assigned single value.
   *
   * @var array
   */
  protected $fields = [
    'field_nurse',
    'field_health_center',
    'field_date_measured',
    'field_execution_date',
    'field_expiration_date',
    'field_batch_number',
    'field_quantity',
    'field_stock_correction_reason',
    'field_stock_supplier',
    'field_stock_update_type',
    'field_notes',
    'field_signature',
    'field_deleted',
  ];

  /**
   * A list of fields that are assigned multiple values.
   *
   * @var array
   */
  protected $multiFields = [];

  /**
   * Fields that represent Entity References. This is a sub list of $fields.
   *
   * @var array
   */
  protected $entityFields = [
    'field_nurse',
    'field_health_center',
  ];

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    // The label is purely decorative.
    unset($public_fields['label']);

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

    unset($public_fields['signature']);
    $public_fields['signature'] = [
      'property' => 'field_signature',
      'process_callbacks' => [
        [$this, 'imageProcess'],
      ],
      'image_styles' => ['signature'],
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

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

    // For the Signature, get to the `file`. We'll convert the `uri`
    // to `field_signature`.
    $query->innerJoin('file_managed', 'f', 'f.fid = field_signature.field_signature_fid');
    $query->addField('f', 'uri');

    return $query;
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      unset($item->label);

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

      if (!empty($item->signature) && !empty($item->uri)) {
        $item->signature = image_style_url('signature', $item->uri);
      }

      unset($item->uri);
    }

    return $items;
  }

}
