<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalMalariaTest.
 */

/**
 * Class HedleyRestfulPrenatalMalariaTest.
 */
class HedleyRestfulPrenatalMalariaTest extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_test_execution_note',
    'field_execution_date',
    'field_test_result',
    'field_blood_smear_ordered',
    'field_blood_smear_result',
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
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->blood_smear_ordered = (bool) $item->blood_smear_ordered;
    }

    return $items;
  }

}
