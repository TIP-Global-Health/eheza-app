<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalSymptomReview.
 */

/**
 * Class HedleyRestfulPrenatalSymptomReview.
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
