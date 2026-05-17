<?php

/**
 * @file
 * Contains HedleyRestfulWellChildPregnancySummary.
 */

/**
 * Class HedleyRestfulWellChildPregnancySummary.
 */
class HedleyRestfulWellChildPregnancySummary extends HedleyRestfulWellChildActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_expected_date_concluded',
    'field_apgar_one_min',
    'field_apgar_five_min',
    'field_weight',
    'field_height',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_pregnancy_summary_signs',
    'field_delivery_complications',
    'field_birth_defects',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_expected_date_concluded',
  ];

}
