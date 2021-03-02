<?php

/**
 * @file
 * Contains HedleyRestfulHealthEducation.
 */

/**
 * Class HedleyRestfulHealthEducation.
 */
class HedleyRestfulHealthEducation extends HedleyRestfulAcuteIllnessActivityBase {

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
