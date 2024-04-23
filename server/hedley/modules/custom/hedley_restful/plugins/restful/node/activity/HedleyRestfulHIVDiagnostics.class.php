<?php

/**
 * @file
 * Contains HedleyRestfulHIVDiagnostics.
 */

/**
 * Class HedleyRestfulHIVDiagnostics.
 */
class HedleyRestfulHIVDiagnostics extends HedleyRestfulHIVActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_positive_result_date',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_hiv_diagnosis_signs',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_positive_result_date',
  ];

}
