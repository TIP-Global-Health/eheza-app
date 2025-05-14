<?php

/**
 * @file
 * Contains HedleyRestfulLastMenstrualPeriods.
 */

/**
 * Class HedleyRestfulLastMenstrualPeriods.
 */
class HedleyRestfulLastMenstrualPeriods extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_last_menstrual_period',
    'field_weight',
    'field_confident',
    'field_not_confident_reason',
    'field_confirmation',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_last_menstrual_period',
  ];

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->confident = (bool) $item->confident;
      $item->confirmation = (bool) $item->confirmation;
    }

    return $items;
  }

}
