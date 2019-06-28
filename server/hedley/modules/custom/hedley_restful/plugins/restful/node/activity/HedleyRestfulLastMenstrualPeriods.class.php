<?php

/**
 * @file
 * Contains HedleyRestfulHeights.
 */

/**
 * Class HedleyRestfulHeights.
 */
class HedleyRestfulHeights extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['last_menstrual_period'] = [
      'property' => 'field_last_menstrual_period',
      'process_callbacks' => [
        [$this, 'renderDate'],
      ],
    ];

    $public_fields['confident'] = [
      'property' => 'field_confident',
    ];

    return $public_fields;
  }

  /**
   * Show the date with date only.
   */
  public function renderDate($date) {
    return date("Y-m-d", $date);
  }

}
