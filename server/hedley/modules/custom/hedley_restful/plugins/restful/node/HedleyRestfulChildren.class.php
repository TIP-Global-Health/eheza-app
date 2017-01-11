<?php

/**
 * @file
 * Contains HedleyRestfulChildren.
 */

/**
 * Class HedleyRestfulChildren.
 */
class HedleyRestfulChildren extends HedleyRestfulEntityBaseNode {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $field_names = [
    ];

    $public_fields['type'] = [
      'callback' => 'static::getType',
    ];

    foreach ($field_names as $field_name) {
      $public_name = str_replace('field_', '', $field_name);
      $public_fields[$public_name] = [
        'property' => $field_name,
      ];
    }

    $public_fields['avatar'] = [
      'property' => 'field_avatar',
      'process_callbacks' => [
        [$this, 'imageProcess'],
      ],
      'image_styles' => ['large', 'thumbnail'],
    ];

    $public_fields['mother'] = [
      'property' => 'nid',
      'process_callbacks' => [
        [$this, 'getMother'],
      ],
    ];

    $public_fields['mother'] = [
      'property' => 'field_mother',
      'resource' => [
        // Bundle name.
        'mother' => [
          // Resource name.
          'name' => 'mothers',
          'full_view' => FALSE,
        ],
      ],
    ];

    foreach (array_keys(field_info_instances($this->getEntityType(), $this->getBundle())) as $field_name) {
      if (strpos($field_name, 'field_date') !== 0) {
        // Not a date field.
        continue;
      }
      $public_name = str_replace('field_', '', $field_name);
      $public_fields[$public_name] = [
        'property' => $field_name,
        'process_callbacks' => [
          [$this, 'convertTimestampToIso8601'],
        ],
      ];
    }

    return $public_fields;
  }

  /**
   * Return the type of the patient.
   *
   * @return string
   *   The type name.
   */
  protected static function getType() {
    return 'child';
  }

  /**
   * Return the mother of the child.
   *
   * @param int $nid
   *   The child node ID.
   *
   * @return int
   *   The Mother node ID.
   */
  protected function getMother($nid) {
    $query = new EntityFieldQuery();
    $result = $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'mother')
      ->fieldCondition('field_children', 'target_id', [$nid], 'IN')
      ->propertyCondition('status', NODE_PUBLISHED)
      // There can be only a single mother.
      ->range(0, 1)
      ->execute();

    if (empty($result['node'])) {
      // In case we somehow don't have a mother.
      return 0;
    }

    return key($result['node']);
  }

}
