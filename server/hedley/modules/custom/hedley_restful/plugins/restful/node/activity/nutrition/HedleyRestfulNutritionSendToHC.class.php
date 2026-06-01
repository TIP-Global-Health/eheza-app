<?php

/**
 * @file
 * Contains HedleyRestfulNutritionSendToHC.
 */

/**
 * Class HedleyRestfulNutritionSendToHC.
 */
class HedleyRestfulNutritionSendToHC extends HedleyRestfulNutritionActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_send_to_hc',
  ];

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_reason_not_sent_to_hc',
  ];

}
