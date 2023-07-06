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
    'field_supplement_type',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_ncda_signs',
  ];

}
