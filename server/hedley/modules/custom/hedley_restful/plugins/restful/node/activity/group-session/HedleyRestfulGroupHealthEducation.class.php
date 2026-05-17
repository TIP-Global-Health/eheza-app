<?php

/**
 * @file
 * Contains HedleyRestfulGroupHealthEducation.
 */

/**
 * Class HedleyRestfulGroupHealthEducation.
 */
class HedleyRestfulGroupHealthEducation extends HedleyRestfulGroupActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_health_education_signs',
  ];


  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_reason_not_given_education',
  ];

}
