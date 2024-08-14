<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalMalariaTest.
 */

/**
 * Class HedleyRestfulPrenatalMalariaTest.
 */
class HedleyRestfulPrenatalMalariaTest extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_test_execution_note',
    'field_execution_date',
    'field_test_result',
    'field_blood_smear_result',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_test_prerequisites',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_execution_date',
  ];

}
