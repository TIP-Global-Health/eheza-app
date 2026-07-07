<?php

/**
 * @file
 * Contains HedleyRestfulMedicationDistributions.
 */

/**
 * Class HedleyRestfulMedicationDistributions.
 */
class HedleyRestfulMedicationDistributions extends HedleyRestfulAcuteIllnessActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_prescribed_medication',
    'field_non_administration_reason',
  ];

}
