<?php

/**
 * @file
 * Contains HedleyRestfulCounselingTopics.
 */

/**
 * Class HedleyRestfulCounselingTopics.
 */
class HedleyRestfulCounselingTopics extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    // The english is in the "label" field.
    $public_fields['kinyarwanda_title'] = [
      'property' => 'field_kinyarwanda_title',
    ];

    return $public_fields;
  }

}
