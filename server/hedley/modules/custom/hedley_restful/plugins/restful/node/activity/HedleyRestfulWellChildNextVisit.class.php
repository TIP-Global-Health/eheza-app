<?php

/**
 * @file
 * Contains HedleyRestfulWellChildNextVisit.
 */

/**
 * Class HedleyRestfulWellChildNextVisit.
 */
class HedleyRestfulWellChildNextVisit extends HedleyRestfulWellChildActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_immunisation_date',
    'field_pediatric_visit_date',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_immunisation_date',
    'field_pediatric_visit_date',
  ];

}
