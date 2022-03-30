<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalRandomBloodSugarTest.
 */

/**
 * Class HedleyRestfulPrenatalRandomBloodSugarTest.
 */
class HedleyRestfulPrenatalRandomBloodSugarTest extends HedleyRestfulPrenatalActivityBase {

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
  protected $dateFields = [
    'field_execution_date',
  ];

}
