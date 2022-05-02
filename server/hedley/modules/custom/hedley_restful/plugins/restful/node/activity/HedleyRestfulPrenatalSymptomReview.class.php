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
  protected $multiFields = [
    'field_prenatal_symptoms',
    'field_prenatal_symptom_questions',
  ];

}
