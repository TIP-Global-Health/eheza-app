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
      'image_styles' => ['large', 'patient-photo'],
    ];

    $public_fields['mother'] = [
      'property' => 'nid',
      'process_callbacks' => [
        [$this, 'getMother'],
      ],
    ];

    $public_fields['sibling'] = [
      'property' => 'nid',
      'process_callbacks' => [
        [$this, 'getSibling'],
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

    $public_fields['gender'] = [
      'property' => 'field_gender',
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
   * Return the sibling of the child.
   *
   * @param int $nid
   *   The child node ID.
   *
   * @return mixed|null
   *   Sibling node ID, or NULL, if none exist.
   */
  public static function getSibling($nid) {
    $mother_nid = self::getMother($nid);
    if (!$mother_nid) {
      return NULL;
    }

    $query = new EntityFieldQuery();
    $result = $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'child')
      ->propertyCondition('nid', $nid, '<>')
      ->propertyCondition('status', NODE_PUBLISHED)
      ->fieldCondition('field_mother', 'target_id', $mother_nid)
      // Child may have up to one sibling.
      ->range(0, 1)
      ->execute();

    return empty($result['node']) ? NULL : key($result['node']);
  }

}
