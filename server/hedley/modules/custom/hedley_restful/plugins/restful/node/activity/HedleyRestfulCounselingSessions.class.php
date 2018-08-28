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
      'resource' => [
        'counseling_topic' => [
          'name' => 'counseling-topics',
          'full_view' => FALSE,
        ],
      ],
    ];

    return $public_fields;
  }

  /**
   * Return the type of the activity.
   *
   * @return string
   *   The type name.
   */
  protected static function getType() {
    return 'counseling_session';
  }

}
