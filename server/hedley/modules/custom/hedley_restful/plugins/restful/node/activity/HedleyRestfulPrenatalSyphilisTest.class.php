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
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_execution_date',
  ];

}
