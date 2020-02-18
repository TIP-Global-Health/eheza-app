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
  protected $dateFields = [
    'field_last_menstrual_period',
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
        [$this, 'renderDate'],
      ],
    ];

    return $public_fields;
  }

  /**
   * Show the date with date only.
   */
  public function renderDate($date) {
    $date = explode(' ', date("Y-m-d", $date));

    return !empty($date[0]) ? $date[0] : NULL;
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
