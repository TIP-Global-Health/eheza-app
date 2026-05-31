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
  protected $fields = [
    'field_preeclampsia_in_family',
  ];

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
