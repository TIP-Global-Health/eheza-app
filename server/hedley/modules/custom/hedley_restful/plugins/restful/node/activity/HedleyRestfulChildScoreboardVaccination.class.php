<?php

/**
 * @file
 * Contains HedleyRestfulChildScoreboardVaccination.
 */

/**
 * Class HedleyRestfulChildScoreboardVaccination.
 */
class HedleyRestfulChildScoreboardVaccination extends HedleyRestfulChildScoreboardActivityBase {
  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_administration_note',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_administered_doses',
    'field_administration_dates',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiDateFields = [
    'field_administration_dates',
  ];

}
