<?php

/**
 * @file
 * Contains HedleyRestfulCounselingSessions.
 */

/**
 * Class HedleyRestfulCounselingSessions.
 */
class HedleyRestfulCounselingSessions extends HedleyRestfulChildActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['topics'] = [
      'property' => 'field_topics',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['timing'] = [
      'property' => 'field_timing',
    ];

    return $public_fields;
  }

}
