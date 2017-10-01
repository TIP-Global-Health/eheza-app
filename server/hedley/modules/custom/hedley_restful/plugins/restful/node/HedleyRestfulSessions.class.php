<?php

/**
 * @file
 * Contains \HedleyRestfulSessions.
 */

/**
 * Class HedleyRestfulSessions.
 */
class HedleyRestfulSessions extends HedleyRestfulEntityBaseNode {

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
          'full_view' => FALSE,
        ],
      ],
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  public function getQueryForList() {
    $request = $this->getRequest();

    $query = parent::getQueryForList();

    // Note that this isn't a security implementation ... we allow the client
    // to specify what day is desired. This helps avoid complications due to
    // differences between server and local time zones (i.e. differences in
    // interpreting what day it is).  The security aspect of this (i.e.
    // preventing data entry after a certain day) will be handled elsewhere.
    //
    // Note that the open_on param should be specified as YYYY-MM-DD.
    if (!empty($request['open_on'])) {
      $openOn = $request['open_on'];

      $query->fieldCondition('field_scheduled_date', 'value', $openOn, '>=');
      $query->fieldCondition('field_scheduled_date', 'value2', $openOn, '<=');
    }

    return $query;
  }

  /**
   * {@inheritdoc}
   */
  public function getQueryCount() {
    $request = $this->getRequest();
    $query = parent::getQueryCount();

    if (!empty($request['open_on'])) {
      $openOn = $request['open_on'];

      $query->fieldCondition('field_scheduled_date', 'value', $openOn, '>=');
      $query->fieldCondition('field_scheduled_date', 'value2', $openOn, '<=');
    }

    return $query;
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
