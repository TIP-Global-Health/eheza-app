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

    $public_fields['english_title'] = [
      'property' => 'field_english_title',
    ];

    $public_fields['kinyarwanda_title'] = [
      'property' => 'field_kinyarwanda_title',
    ];

    // No need for the label field because we are using translations fields for
    // the title.
    unset($public_fields['label']);
    unset($public_fields['self']);
    unset($public_fields['created']);

    return $public_fields;
  }

}
