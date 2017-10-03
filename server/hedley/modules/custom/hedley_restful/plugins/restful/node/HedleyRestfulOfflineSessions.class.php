<?php

/**
 * @file
 * Contains \HedleyRestfulOfflineSessions.
 *
 * These are the same sessions as in the `sessions` endpoint, but specialized
 * to support going offline to take measurements and then upload those
 * measurements. So, everything is provided that is needed for that purpose,
 * unlike the `sessions` endpoint, which assumes that you're basically online.
 */

/**
 * Class HedleyRestfulOfflineSessions.
 */
class HedleyRestfulOfflineSessions extends HedleyRestfulEntityBaseNode {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['type'] = [
      'callback' => 'static::getType',
    ];

    $public_fields['scheduled_date'] = [
      'property' => 'field_scheduled_date',
      'process_callbacks' => [
        [$this, 'renderDate'],
      ],
    ];

    $public_fields['clinic'] = [
      'property' => 'field_clinic',
      'resource' => [
        'clinic' => [
          'name' => 'clinics',
          'full_view' => TRUE,
        ],
      ],
    ];

    return $public_fields;
  }

  /**
   * Return the type of the entity.
   *
   * @return string
   *   The type name.
   */
  protected static function getType() {
    return 'session';
  }

  /**
   * Show the scheduled_date with date only.
   */
  public function renderDate($date) {
    return [
      'value' => $date['value'] ? hedley_restful_timestamp_only_date($date['value']) : NULL,
      'value2' => $date['value2'] ? hedley_restful_timestamp_only_date($date['value2']) : NULL,
    ];
  }

}
