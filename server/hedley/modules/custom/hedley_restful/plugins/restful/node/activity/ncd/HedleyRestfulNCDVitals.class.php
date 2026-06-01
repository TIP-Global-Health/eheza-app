<?php

/**
 * @file
 * Contains HedleyRestfulNCDVitals.
 */

/**
 * Class HedleyRestfulNCDVitals.
 */
class HedleyRestfulNCDVitals extends HedleyRestfulNCDActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_sys',
    'field_dia',
    'field_heart_rate',
    'field_respiratory_rate',
    'field_body_temperature',
  ];

}
