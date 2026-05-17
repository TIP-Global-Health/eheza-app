<?php

/**
 * @file
 * Contains HedleyRestfulNCDOutsideCare.
 */

/**
 * Class HedleyRestfulNCDOutsideCare.
 */
class HedleyRestfulNCDOutsideCare extends HedleyRestfulNCDActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_outside_care_signs',
    'field_medical_conditions',
    'field_outside_care_medications',
  ];

}
