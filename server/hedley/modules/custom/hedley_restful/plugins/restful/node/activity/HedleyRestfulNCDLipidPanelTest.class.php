<?php

/**
 * @file
 * Contains HedleyRestfulNCDLipidPanelTest.
 */

/**
 * Class HedleyRestfulNCDLipidPanelTest.
 */
class HedleyRestfulNCDLipidPanelTest extends HedleyRestfulNCDActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_test_execution_note',
    'field_execution_date',
    'field_unit_of_measurement',
    'field_total_cholesterol',
    'field_ldl_cholesterol',
    'field_hdl_cholesterol',
    'field_triglycerides',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_execution_date',
  ];

}
