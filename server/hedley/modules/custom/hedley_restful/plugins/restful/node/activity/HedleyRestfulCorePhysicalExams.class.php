<?php

/**
 * @file
 * Contains HedleyRestfulCorePhysicalExames.
 */

/**
 * Class HedleyRestfulCorePhysicalExams.
 */
class HedleyRestfulCorePhysicalExams extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_heart_murmur',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_head_hair',
    'field_eyes',
    'field_heart',
    'field_neck',
    'field_lungs',
    'field_abdomen',
    'field_hands',
    'field_legs',
  ];

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->heart_murmur = (bool) $item->heart_murmur;
    }

    return $items;
  }

}
