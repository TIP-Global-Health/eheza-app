<?php

/**
 * @file
 * Contains HedleyRestfulWeights.
 */

/**
 * Class HedleyRestfulWeights.
 */
class HedleyRestfulMothers extends HedleyRestfulEntityBaseNode {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['type'] = [
      'callback' => 'static::getType',
    ];

    $date_field_names = [];

    foreach ($date_field_names as $field_name) {
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
      'image_styles' => ['large', 'patient-photo'],
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

    $public_fields['children'] = [
      'property' => 'nid',
      'process_callbacks' => [
        [$this, 'getChildren'],
      ],
    ];

    $public_fields['lastExamination'] = [
      'property' => 'nid',
      'process_callbacks' => [
        [$this, 'lastExamination'],
      ],
    ];

    return $public_fields;
  }

  /**
   * Return the type of the patient.
   *
   * @return string
   *   The type name.
   */
  protected static function getType() {
    return 'mother';
  }

  /**
   * Get the Child(s) node IDs.
   *
   * @param int $nid
   *   The Mother node Id.
   *
   * @return array
   *   Array with the children node IDs.
   */
  public static function getChildren($nid) {
    $query = new EntityFieldQuery();
    $result = $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'child')
      ->fieldCondition('field_mother', 'target_id', $nid)
      ->propertyCondition('status', NODE_PUBLISHED)
      // Prevent any abuse.
      ->range(0, 50)
      ->execute();

    return !empty($result['node']) ? array_keys($result['node']) : [];
  }

  /**
   * Return the group of the mother.
   *
   * @param int $nid
   *   The mother node ID.
   *
   * @return int
   *   The group node ID.
   */
  public static function getGroup($nid) {
    $mother_wrapper = entity_metadata_wrapper('node', $nid);
    return $mother_wrapper->field_group->getIdentifier();
  }

  /**
   * Fetches the Node Id the last assessment.
   *
   * @param int $nid
   *   Node Id of a Mother or a Child.
   *
   * @return int|null
   *   The Examination Node ID or NULL if not found.
   */
  protected function lastExamination($nid) {
    $group_nid = self::getGroup($nid);

    $query = new EntityFieldQuery();
    $result = $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'examination')
      ->fieldCondition('field_group', 'target_id', $group_nid)
      ->propertyOrderBy('created', 'DESC')
      // There can be only a single examination.
      ->range(0, 1)
      ->execute();

    if (empty($result['node'])) {
      // No Examination for this Mother yet.
      return NULL;
    }

    return key($result['node']);
  }

}
