<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalMentalHealth.
 */

/**
 * Class HedleyRestfulPrenatalMentalHealth.
 */
class HedleyRestfulPrenatalMentalHealth extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_specialist_at_hc',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_mental_health_signs',
  ];

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->specialist_at_hc = (bool) $item->specialist_at_hc;
    }

    return $items;
  }

}
