<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalHemoglobinTest.
 */

/**
 * Class HedleyRestfulPrenatalHemoglobinTest.
 */
class HedleyRestfulPrenatalHemoglobinTest extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_test_execution_note',
    'field_execution_date',
    'field_hemoglobin_count',
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
