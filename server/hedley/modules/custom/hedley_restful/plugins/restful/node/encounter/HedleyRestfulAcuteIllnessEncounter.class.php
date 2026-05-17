<?php

/**
 * @file
 * Contains \HedleyRestfulAcuteIllnessEncounter.
 */

/**
 * Class HedleyRestfulAcuteIllnessEncounter.
 */
class HedleyRestfulAcuteIllnessEncounter extends HedleyRestfulIndividualEncounter {

  /**
   * A list of fields that are assigned single value.
   *
   * @var array
   */
  protected $fields = [
    'field_acute_illness_diagnosis',
    'field_sequence_number',
    'field_ai_encounter_type',
  ];

}
