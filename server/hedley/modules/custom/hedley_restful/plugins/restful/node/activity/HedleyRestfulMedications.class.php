<?php

/**
 * @file
 * Contains HedleyRestfulMedications.
 */

/**
 * Class HedleyRestfulMedications.
 */
class HedleyRestfulMedications extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_medication',
    'field_hiv_treatment',
    'field_hypertension_treatment',
    'field_malaria_treatment',
    'field_anemia_treatment',
    'field_syphilis_treatment',
  ];

}
