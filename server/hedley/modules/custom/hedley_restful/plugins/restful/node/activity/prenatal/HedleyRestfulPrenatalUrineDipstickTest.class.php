<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalUrineDipstickTest.
 */

/**
 * Class HedleyRestfulPrenatalUrineDipstickTest.
 */
class HedleyRestfulPrenatalUrineDipstickTest extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_test_variant',
    'field_test_execution_note',
    'field_execution_date',
    'field_protein',
    'field_ph',
    'field_glucose',
    'field_leukocytes',
    'field_nitrite',
    'field_urobilinogen',
    'field_haemoglobin',
    'field_ketone',
    'field_bilirubin',
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
