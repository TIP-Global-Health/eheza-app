<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalLabsResults.
 */

/**
 * Class HedleyRestfulPrenatalLabsResults.
 */
class HedleyRestfulPrenatalLabsResults extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_date_concluded',
    'field_patient_notified',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_performed_tests',
    'field_completed_tests',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_date_concluded',
  ];

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->patient_notified = (bool) $item->patient_notified;
    }

    return $items;
  }

}