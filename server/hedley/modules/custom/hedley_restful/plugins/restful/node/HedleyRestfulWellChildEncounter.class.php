<?php

/**
 * @file
 * Contains \HedleyRestfulWellChildEncounter.
 */

/**
 * Class HedleyRestfulWellChildEncounter.
 */
class HedleyRestfulWellChildEncounter extends HedleyRestfulIndividualEncounter {

  /**
   * A list of fields that are assigned single value.
   *
   * @var array
   */
  protected $fields = [
    'field_well_child_encounter_type',
  ];

  /**
   * A list of fields that are assigned multiple values.
   *
   * @var array
   */
  protected $multiFields = [
    'field_encounter_notes',
    'field_encounter_warnings',
  ];

}
