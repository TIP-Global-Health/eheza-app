<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalOutsideCare.
 */

/**
 * Class HedleyRestfulPrenatalOutsideCare.
 */
class HedleyRestfulPrenatalOutsideCare extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_outside_care_signs',
    'field_prenatal_diagnoses',
    'field_outside_care_medications',
  ];

}
