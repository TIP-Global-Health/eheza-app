<?php

/**
 * @file
 * Contains HedleyRestfulWellChildNCDA.
 */

/**
 * Class HedleyRestfulWellChildNCDA.
 */
class HedleyRestfulWellChildNCDA extends HedleyRestfulWellChildActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_weight',
    'field_anc_visits',
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
