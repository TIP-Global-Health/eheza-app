<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalSyphilisTest.
 */

/**
 * Class HedleyRestfulPrenatalSyphilisTest.
 */
class HedleyRestfulPrenatalSyphilisTest extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_test_execution_note',
    'field_execution_date',
    'field_test_result',
    'field_originating_encounter',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_illness_symptoms',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_execution_date',
    'field_originating_encounter',
  ];

}
