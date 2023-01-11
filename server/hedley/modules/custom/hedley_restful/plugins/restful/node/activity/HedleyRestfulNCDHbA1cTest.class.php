<?php

/**
 * @file
 * Contains HedleyRestfulNCDHbA1cTest.
 */

/**
 * Class HedleyRestfulNCDHbA1cTest.
 */
class HedleyRestfulNCDHbA1cTest extends HedleyRestfulNCDActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_test_execution_note',
    'field_execution_date',
    'field_hba1c_result',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_execution_date',
  ];

}
