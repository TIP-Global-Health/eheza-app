<?php

/**
 * @file
 * Contains HedleyRestfulTuberculosisTreatmentReview.
 */

/**
 * Class HedleyRestfulTuberculosisTreatmentReview.
 */
class HedleyRestfulTuberculosisTreatmentReview extends HedleyRestfulTuberculosisActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_reason_for_not_taking',
    'field_missed_doses',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_treatment_ongoing',
    'field_adverse_events',
  ];

}
