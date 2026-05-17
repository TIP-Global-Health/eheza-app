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
    'field_birth_weight',
    'field_receive_option',
    'field_stunting_level',
    'field_weight',
    'field_muac',
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
