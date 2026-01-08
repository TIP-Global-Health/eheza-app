<?php

/**
 * @file
 * Contains HedleyRestfulUltrasound.
 */

/**
 * Class HedleyRestfulUltrasound.
 */
class HedleyRestfulUltrasound extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_gestational_age_weeks',
    'field_gestational_age_days',
    'field_edd',
    'field_viable_pregnancy',
    'field_number_of_fetuses',
    'field_pregnancy_location',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_edd',
  ];

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->viable_pregnancy = (bool) $item->viable_pregnancy;
    }

    return $items;
  }

}
