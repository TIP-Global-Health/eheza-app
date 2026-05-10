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
    'field_next_visit_date',
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

  /**
   * A list of fields that are dates. This is a sub list of $fields.
   *
   * @var array
   */
  protected $dateFields = [
    'field_next_visit_date',
  ];

}
