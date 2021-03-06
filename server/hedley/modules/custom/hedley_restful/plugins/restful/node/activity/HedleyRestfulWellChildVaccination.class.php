<?php

/**
 * @file
 * Contains HedleyRestfulWellChildVaccination.
 */

/**
 * Class HedleyRestfulWellChildVaccination.
 */
class HedleyRestfulWellChildVaccination extends HedleyRestfulWellChildActivityBase {
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
