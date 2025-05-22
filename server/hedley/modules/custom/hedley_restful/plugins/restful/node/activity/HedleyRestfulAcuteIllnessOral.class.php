<?php

/**
 * @file
 * Contains HedleyRestfulAcuteIllnessVitals.
 */

/**
 * Class HedleyRestfulAcuteIllnessVitals.
 */
class HedleyRestfulAcuteIllnessOral extends HedleyRestfulAcuteIllnessActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_mouth_ulcer_period',
    'field_toothache_period',
    'field_swollen_gums_period',
  ];

}
