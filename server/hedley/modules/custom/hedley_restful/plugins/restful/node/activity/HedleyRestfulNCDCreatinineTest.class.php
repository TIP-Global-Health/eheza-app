<?php

/**
 * @file
 * Contains HedleyRestfulNCDCreatinineTest.
 */

/**
 * Class HedleyRestfulNCDCreatinineTest.
 */
class HedleyRestfulNCDCreatinineTest extends HedleyRestfulNCDActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_test_execution_note',
    'field_execution_date',
    'field_creatinine_result',
    'field_urea_result',
    'field_nitorogen_result',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_execution_date',
  ];

}
