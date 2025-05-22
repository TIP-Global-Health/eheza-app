<?php

/**
 * @file
 * Contains HedleyRestfulAcuteIllnessVitals.
 */

/**
 * Class HedleyRestfulAcuteIllnessVitals.
 */
class HedleyRestfulAcuteIllnessENT extends HedleyRestfulAcuteIllnessActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_ear_pain_period',
    'field_ear_pus_discharge_period',
    'field_sore_throat_period',
    'field_difficult_swallow_period',
  ];

}
