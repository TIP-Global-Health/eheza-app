<?php

/**
 * @file
 * Contains HedleyRestfulAttendances.
 */

/**
 * Class HedleyRestfulAttendances.
 */
class HedleyRestfulAttendances extends HedleyRestfulGroupActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_attended',
  ];

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->attended = (bool) $item->attended;
    }

    return $items;
  }

}
