<?php

/**
 * @file
 * Contains HedleyRestfulCounselingSchedule.
 */

/**
 * Class HedleyRestfulCounselingSchedule.
 */
class HedleyRestfulCounselingSchedule extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['timing'] = [
      'property' => 'field_timing',
    ];

    $public_fields['topics'] = [
      'property' => 'field_topics',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

}
