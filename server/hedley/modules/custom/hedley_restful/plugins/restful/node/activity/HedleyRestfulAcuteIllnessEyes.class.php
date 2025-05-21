<?php

/**
 * @file
 * Contains HedleyRestfulAcuteIllnessVitals.
 */

/**
 * Class HedleyRestfulAcuteIllnessVitals.
 */
class HedleyRestfulAcuteIllnessEyes extends HedleyRestfulAcuteIllnessActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_pus_from_eye_period',
    'field_swollen_eyes_period',
    'field_yellow_eyes_period',
    'field_red_eyes_period',
    'field_cloudy_appearance_period',
    'field_eye_irritation_period',
  ];

}
