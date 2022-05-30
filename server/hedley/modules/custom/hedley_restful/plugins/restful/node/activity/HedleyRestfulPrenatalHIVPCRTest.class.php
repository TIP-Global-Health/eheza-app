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
    'field_test_result',
    'field_hiv_level_undetectable',
    'field_hiv_viral_load',
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
      $item->hiv_level_undetectable = (bool) $item->hiv_level_undetectable;
    }

    return $items;
  }

}
