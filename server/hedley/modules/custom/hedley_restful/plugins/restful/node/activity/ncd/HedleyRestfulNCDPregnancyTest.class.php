<?php

/**
 * @file
 * Contains HedleyRestfulNCDPregnancyTest.
 */

/**
 * Class HedleyRestfulNCDPregnancyTest.
 */
class HedleyRestfulNCDPregnancyTest extends HedleyRestfulNCDActivityBase {

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
