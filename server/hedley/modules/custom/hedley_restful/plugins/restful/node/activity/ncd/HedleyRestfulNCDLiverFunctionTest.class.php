<?php

/**
 * @file
 * Contains HedleyRestfulNCDLiverFunctionTest.
 */

/**
 * Class HedleyRestfulNCDLiverFunctionTest.
 */
class HedleyRestfulNCDLiverFunctionTest extends HedleyRestfulNCDActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_test_execution_note',
    'field_execution_date',
    'field_alt_result',
    'field_ast_result',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_execution_date',
  ];

}
