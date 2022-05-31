<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalHIVPCRTest.
 */

/**
 * Class HedleyRestfulPrenatalHIVPCRTest.
 */
class HedleyRestfulPrenatalHIVPCRTest extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_test_execution_note',
    'field_execution_date',
    'field_hiv_viral_load_status',
    'field_hiv_viral_load',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_execution_date',
  ];

}
