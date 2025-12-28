<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalMedicationDistribution.
 */

/**
 * Class HedleyRestfulPrenatalMedicationDistribution.
 */
class HedleyRestfulPrenatalMedicationDistribution extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_prescribed_medication',
    'field_non_administration_reason',
    'field_recommended_treatment',
    'field_avoiding_guidance_reason',
    'field_reinforce_treatment_signs',
  ];

}
