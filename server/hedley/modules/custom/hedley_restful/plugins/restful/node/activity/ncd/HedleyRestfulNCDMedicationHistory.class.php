<?php

/**
 * @file
 * Contains HedleyRestfulNCDMedicationHistory.
 */

/**
 * Class HedleyRestfulNCDMedicationHistory.
 */
class HedleyRestfulNCDMedicationHistory extends HedleyRestfulNCDActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_causing_hypertension',
    'field_treating_hypertension',
    'field_treating_diabetes',
  ];

}
