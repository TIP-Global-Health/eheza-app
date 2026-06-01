<?php

/**
 * @file
 * Contains HedleyRestfulWellChildVitals.
 */

/**
 * Class HedleyRestfulWellChildVitals.
 */
class HedleyRestfulWellChildVitals extends HedleyRestfulWellChildActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_respiratory_rate',
    'field_body_temperature',
  ];

}
