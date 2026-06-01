<?php

/**
 * @file
 * Contains HedleyRestfulDangerSigns.
 */

/**
 * Class HedleyRestfulDangerSigns.
 */
class HedleyRestfulDangerSigns extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_danger_signs',
    'field_postpartum_mother',
    'field_postpartum_child',
  ];

}
