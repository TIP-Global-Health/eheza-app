<?php

/**
 * @file
 * Contains HedleyRestfulNCDSocialHistory.
 */

/**
 * Class HedleyRestfulNCDSocialHistory.
 */
class HedleyRestfulNCDSocialHistory extends HedleyRestfulNCDActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_food_group',
    'field_beverages_per_week',
    'field_cigarettes_per_week',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_ncd_social_history_signs',
  ];

}
