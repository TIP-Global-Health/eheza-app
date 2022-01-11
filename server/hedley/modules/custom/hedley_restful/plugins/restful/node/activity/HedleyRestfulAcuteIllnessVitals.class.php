<?php

/**
 * @file
 * Contains HedleyRestfulAcuteIllnessVitals.
 */

/**
 * Class HedleyRestfulAcuteIllnessVitals.
 */
class HedleyRestfulAcuteIllnessVitals extends HedleyRestfulAcuteIllnessActivityBase {

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
