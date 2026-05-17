<?php

/**
 * @file
 * Contains HedleyRestfulVitals.
 */

/**
 * Class HedleyRestfulVitals.
 */
class HedleyRestfulVitals extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_sys',
    'field_dia',
    'field_heart_rate',
    'field_respiratory_rate',
    'field_body_temperature',
    'field_sys_repeated',
    'field_dia_repeated',
  ];

}
