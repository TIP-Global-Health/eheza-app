<?php

/**
 * @file
 * Contains HedleyRestfulNutritionNCDA.
 */

/**
 * Class HedleyRestfulNutritionNCDA.
 */
class HedleyRestfulChildScoreboardNCDA extends HedleyRestfulChildScoreboardActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_weight',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_ncda_signs',
    'field_anc_visits_dates',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiDateFields = [
    'field_anc_visits_dates',
  ];

}