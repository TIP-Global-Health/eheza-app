<?php

/**
 * @file
 * Contains HedleyRestfulNCDMedicationDistribution.
 */

/**
 * Class HedleyRestfulNCDMedicationDistribution.
 */
class HedleyRestfulNCDMedicationDistribution extends HedleyRestfulNCDActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_recommended_treatment',
    'field_ncd_guidance',
  ];

}
