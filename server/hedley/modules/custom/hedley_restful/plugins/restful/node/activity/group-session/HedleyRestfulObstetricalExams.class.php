<?php

/**
 * @file
 * Contains HedleyRestfulObstetricalExams.
 */

/**
 * Class HedleyRestfulObstetricalExams.
 */
class HedleyRestfulObstetricalExams extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_fundal_palpable',
    'field_fundal_height',
    'field_fetal_presentation',
    'field_fetal_movement',
    'field_fetal_heart_rate',
    'field_c_section_scar',
  ];

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->fetal_movement = (bool) $item->fetal_movement;
      $item->fundal_palpable = (bool) $item->fundal_palpable;
    }

    return $items;
  }

}
