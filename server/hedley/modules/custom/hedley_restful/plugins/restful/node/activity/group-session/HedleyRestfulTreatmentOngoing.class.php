<?php

/**
 * @file
 * Contains HedleyRestfulTreatmentOngoing.
 */

/**
 * Class HedleyRestfulTreatmentOngoing.
 */
class HedleyRestfulTreatmentOngoing extends HedleyRestfulAcuteIllnessActivityBase {

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
