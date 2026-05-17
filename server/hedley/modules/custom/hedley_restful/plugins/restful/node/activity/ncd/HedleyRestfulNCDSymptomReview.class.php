<?php

/**
 * @file
 * Contains HedleyRestfulNCDSymptomReview.
 */

/**
 * Class HedleyRestfulNCDSymptomReview.
 */
class HedleyRestfulNCDSymptomReview extends HedleyRestfulNCDActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_ncd_group1_symptoms',
    'field_ncd_group2_symptoms',
    'field_ncd_pain_symptoms',
  ];

}
