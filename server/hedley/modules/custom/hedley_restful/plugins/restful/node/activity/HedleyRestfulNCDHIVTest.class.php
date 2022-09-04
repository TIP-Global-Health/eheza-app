<?php

/**
 * @file
 * Contains HedleyRestfulNCDHIVTest.
 */

/**
 * Class HedleyRestfulNCDHIVTest.
 */
class HedleyRestfulNCDHIVTest extends HedleyRestfulNCDActivityBase {

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
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_execution_date',
  ];

}
