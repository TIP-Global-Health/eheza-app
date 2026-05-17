<?php

/**
 * @file
 * Contains HedleyRestfulNCDFamilyHistory.
 */

/**
 * Class HedleyRestfulNCDFamilyHistory.
 */
class HedleyRestfulNCDFamilyHistory extends HedleyRestfulNCDActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_ncd_family_history_signs',
    'field_hypertension_predecessors',
    'field_heart_problem_predecessors',
    'field_diabetes_predecessors',
  ];

}
