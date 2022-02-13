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
  ];

}
