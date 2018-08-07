<?php

/**
 * @file
 * Contains HedleyRestfulCounselingSchedule.
 */

/**
 * Class HedleyRestfulCounselingSchedule.
 */
class HedleyRestfulCounselingSchedule extends HedleyRestfulEntityBaseNode {

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
    ];

    return $public_fields;
  }

}
