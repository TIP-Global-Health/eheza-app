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
    'reason_for_not_taking',
    'missed_doses',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_treatment_ongoing',
  ];

}
