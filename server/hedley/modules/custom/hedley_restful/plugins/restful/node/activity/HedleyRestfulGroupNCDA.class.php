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
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_ncda_signs',
  ];

}
