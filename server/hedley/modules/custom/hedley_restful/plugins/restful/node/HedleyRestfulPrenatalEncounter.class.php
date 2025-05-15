<?php

/**
 * @file
 * Contains \HedleyRestfulPrenatalEncounter.
 */

/**
 * Class HedleyRestfulPrenatalEncounter.
 */
class HedleyRestfulPrenatalEncounter extends HedleyRestfulIndividualEncounter {

  /**
   * A list of fields that are assigned single value.
   *
   * @var array
   */
  protected $fields = [
    'field_prenatal_encounter_type',
  ];

  /**
   * A list of fields that are assigned multiple values.
   *
   * @var array
   */
  protected $multiFields = [
    'field_prenatal_diagnoses',
    'field_past_prenatal_diagnoses',
    'field_prenatal_indicators',
  ];

}
