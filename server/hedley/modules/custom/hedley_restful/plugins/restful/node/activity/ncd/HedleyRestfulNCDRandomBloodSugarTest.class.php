<?php

/**
 * @file
 * Contains HedleyRestfulNCDRandomBloodSugarTest.
 */

/**
 * Class HedleyRestfulNCDRandomBloodSugarTest.
 */
class HedleyRestfulNCDRandomBloodSugarTest extends HedleyRestfulNCDActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_test_execution_note',
    'field_execution_date',
    'field_sugar_count',
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
