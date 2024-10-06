<?php

/**
 * @file
 * Contains HedleyRestfulMedicalHistories.
 */

/**
 * Class HedleyRestfulMedicalHistories.
 */
class HedleyRestfulMedicalHistories extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_medical_history',
    'field_physical_condition_history',
    'field_infectious_disease_history',
    'field_mental_health_issues',
  ];

}
