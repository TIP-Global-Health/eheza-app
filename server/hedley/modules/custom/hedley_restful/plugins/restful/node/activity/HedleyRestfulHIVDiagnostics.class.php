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
    'field_hiv_diagnosis',
    'field_positive_result_date',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_positive_result_date',
  ];

}
