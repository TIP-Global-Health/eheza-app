<?php

/**
 * @file
 * Contains HedleyRestfulCovidTesting.
 */

/**
 * Class HedleyRestfulCovidTesting.
 */
class HedleyRestfulCovidTesting extends HedleyRestfulAcuteIllnessActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_rapid_test_result',
    'field_administration_note',
  ];

}
