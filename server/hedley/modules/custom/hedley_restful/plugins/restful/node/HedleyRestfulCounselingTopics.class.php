<?php

/**
 * @file
 * Contains HedleyRestfulCounselingTopics.
 */

/**
 * Class HedleyRestfulCounselingTopics.
 */
class HedleyRestfulCounselingTopics extends HedleyRestfulEntityBaseNode {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    // The english is in the "label" field
    $public_fields['kinyarwanda_title'] = [
      'property' => 'field_kinyarwanda_title',
    ];

    unset($public_fields['self']);
    unset($public_fields['created']);

    return $public_fields;
  }

}
