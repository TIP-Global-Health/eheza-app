<?php

/**
 * @file
 * Contains HedleyRestfulGroupNCDA.
 */

/**
 * Class HedleyRestfulGroupNCDA.
 */
class HedleyRestfulGroupNCDA extends HedleyRestfulGroupActivityBase {

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
