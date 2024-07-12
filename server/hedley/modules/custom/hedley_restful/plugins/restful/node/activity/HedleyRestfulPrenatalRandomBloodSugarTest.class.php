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
    'field_originating_encounter',
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

  /**
   * {@inheritdoc}
   */
  protected $entityFields = [
    'field_originating_encounter',
  ];

}
