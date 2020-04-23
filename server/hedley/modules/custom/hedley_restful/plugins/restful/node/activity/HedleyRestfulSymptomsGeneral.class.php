<?php

/**
 * @file
 * Contains HedleyRestfulSymptomsGeneral.
 */

/**
 * Class HedleyRestfulSymptomsGeneral.
 */
class HedleyRestfulSymptomsGeneral extends HedleyRestfulAcuteIllnessActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_fever_period',
    'field_chills_period',
    'field_night_sweats_period',
    'field_body_aches_period',
    'field_headache_period',
  ];

}
