<?php

/**
 * @file
 * Contains \HedleyRestfulNCDEncounter.
 */

/**
 * Class HedleyRestfulNCDEncounter.
 */
class HedleyRestfulNCDEncounter extends HedleyRestfulIndividualEncounter {

  /**
   * A list of fields that are assigned multiple values.
   *
   * @var array
   */
  protected $multiFields = [
    'field_ncd_diagnoses',
  ];

}
