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

    $field_names = [];

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

    $public_fields['lastExamination'] = [
      'property' => 'nid',
      'process_callbacks' => [
        [$this, 'lastExamination'],
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
  public static function getMother($nid) {
    $child_wrapper = entity_metadata_wrapper('node', $nid);
    return $child_wrapper->field_mother->getIdentifier();
  }

  /**
   * Return the group of the child.
   *
   * @param int $nid
   *   The child node ID.
   *
   * @return int
   *   The Group node ID.
   */
  public static function getGroup($nid) {
    $mother_nid = self::getMother($nid);
    if (!$mother_nid) {
      return 0;
    }

    $mother_wrapper = entity_metadata_wrapper('node', $mother_nid);
    return $mother_wrapper->field_group->getIdentifier();
  }

  /**
   * Fetches the measurement values of the last completed assessment.
   *
   * @param int $nid
   *   Node Id of a Mother or a Child.
   *
   * @return array
   *   Associative array of the measurement data.
   */
  protected function lastExamination($nid) {
    $group_nid = self::getGroup($nid);

    $query = new EntityFieldQuery();
    $result = $query
      ->propertyOrderBy('created', 'DESC')
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'examination')
      ->fieldCondition('field_group', 'target_id', $group_nid)
      // There can be only a single examination.
      ->range(0, 1)
      ->execute();

    if (empty($result['node'])) {
      // In case we somehow don't have a mother.
      return 0;
    }

    $last_examination_nid = key($result['node']);

    $examination = [];

    $query = new EntityFieldQuery();
    $result = $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'weight')
      ->fieldCondition('field_child', 'target_id', $nid)
      ->fieldCondition('field_examination', 'target_id', $last_examination_nid)
      // There can be only a single examination.
      ->range(0, 1)
      ->execute();

    $weight_wrapper = entity_metadata_wrapper('node', key($result['node']));

    $examination['weight'] = $weight_wrapper->field_weight->value();

    return $examination;
  }

}
