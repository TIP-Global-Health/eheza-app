<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalGUExam.
 */

/**
 * Class HedleyRestfulPrenatalGUExam.
 */
class HedleyRestfulPrenatalGUExam extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_vaginal_exam_signs',
    'field_postpartum_healing_problem',
    'field_gu_exam_signs',
  ];

}
