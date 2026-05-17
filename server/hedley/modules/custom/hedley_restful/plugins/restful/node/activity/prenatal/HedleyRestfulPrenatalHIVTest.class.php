<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalHIVTest.
 */

/**
 * Class HedleyRestfulPrenatalHIVTest.
 */
class HedleyRestfulPrenatalHIVTest extends HedleyRestfulPrenatalActivityBase {

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
  protected $multiFields = [
    'field_hiv_signs',
    'field_test_prerequisites',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_execution_date',
  ];

}
