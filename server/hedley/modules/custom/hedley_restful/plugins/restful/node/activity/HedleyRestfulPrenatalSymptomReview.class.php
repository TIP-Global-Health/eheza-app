<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalSymptoms.
 */

/**
 * Class HedleyRestfulPrenatalSymptoms.
 */
class HedleyRestfulPrenatalSymptomReview extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_flank_pain_sign',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_prenatal_symptoms',
    'field_prenatal_symptom_questions',
  ];

}
