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
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_ncda_signs',
  ];

}
