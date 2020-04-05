<?php

/**
 * @file
 * Contains HedleyRestfulSymptomsGI.
 */

/**
 * Class HedleyRestfulSymptomsGI.
 */
class HedleyRestfulSymptomsGI extends HedleyRestfulAcuteIllnessActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_bloody_diarrhea_period',
    'field_non_bloody_diarrhea_period',
    'field_nausea_period',
    'field_vomiting_period',
    'field_abdominal_pain_period',
  ];

}
