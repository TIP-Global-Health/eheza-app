<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalBloodGpRsTest.
 */

/**
 * Class HedleyRestfulPrenatalBloodGpRsTest.
 */
class HedleyRestfulPrenatalBloodGpRsTest extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_test_execution_note',
    'field_execution_date',
    'field_blood_group',
    'field_rhesus',
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
