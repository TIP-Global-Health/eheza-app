<?php

/**
 * @file
 * Contains HedleyRestfulBreastExams.
 */

/**
 * Class HedleyRestfulBreastExams.
 */
class HedleyRestfulBreastExams extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_breast_self_exam',
    'field_discharge_type',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_breast',
  ];

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->breast_self_exam = (bool) $item->breast_self_exam;
    }

    return $items;
  }

}
