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
    'field_confident',
  ];

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    unset($public_fields['last_menstrual_period']);

    $public_fields['last_menstrual_period'] = [
      'property' => 'field_last_menstrual_period',
      'process_callbacks' => [
        [$this, 'renderDate2'],
      ],
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->confident = (bool) $item->confident;
    }

    return $items;
  }

}
