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
    'field_review_state',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_performed_tests',
    'field_completed_tests',
    'field_tests_with_follow_up',
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
