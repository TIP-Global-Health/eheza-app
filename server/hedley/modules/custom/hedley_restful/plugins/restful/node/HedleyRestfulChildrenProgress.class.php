<?php

/**
 * @file
 * Contains HedleyRestfulChildrenProgress.
 */

/**
 * Class HedleyRestfulChildrenProgress.
 */
class HedleyRestfulChildrenProgress extends HedleyRestfulEntityBaseNode {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['type'] = [
      'callback' => 'static::getType',
    ];

    $public_fields['date_birth'] = [
      'property' => 'field_date_birth',
      'process_callbacks' => [
        [$this, 'convertTimestampToIso8601'],
      ],
    ];

    $public_fields['gender'] = [
      'property' => 'field_gender',
    ];

    $public_fields['examinations'] = [
      'property' => 'nid',
      'process_callbacks' => [
        [$this, 'getChildExaminations'],
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
    return 'child-progress';
  }

  /**
   * Return child examinations.
   *
   * @param int $nid
   *   The child node ID.
   *
   * @return array|null
   *   Examinations array, or NULL if none exist.
   */
  protected function getChildExaminations($nid) {
    $child_measurement_types = [
      'weight' => 'weight',
      'height' => 'height',
      'muac' => 'muac',
      'photo' => 'photo',
      'nutrition' => 'nutrition_signs',
    ];

    $query = new EntityFieldQuery();
    $result = $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', array_keys($child_measurement_types), 'IN')
      ->fieldCondition('field_child', 'target_id', $nid)
      ->propertyOrderBy('nid', 'DESC')
      ->range(0, 100)
      ->execute();

    if (empty($result['node'])) {
      return NULL;
    }

    $child_examination['id'] = 'dummy';
    foreach(array_keys($child_measurement_types) as $measurement_type) {
      $child_examination[$measurement_type] = NULL;
    }

    foreach($result['node'] as $measurement) {
      if (is_null($child_examination[$measurement->type])) {
        $wrapper = entity_metadata_wrapper('node', $measurement->nid);
        $field = 'field_' . $child_measurement_types[$measurement->type];

        if ($measurement->type == 'photo') {
          $child_examination[$measurement->type] = $this->imageProcess($wrapper->$field->value());
          $child_examination[$measurement->type]['image_styles'] = ['thumbnail'];
        }
        else {
          $child_examination[$measurement->type] = $wrapper->$field->value();
        }
      }
    }

    return [$child_examination];
  }

}
