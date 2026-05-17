<?php

/**
 * @file
 * Contains HedleyRestfulAcuteFindings.
 */

/**
 * Class HedleyRestfulAcuteFindings.
 */
class HedleyRestfulAcuteFindings extends HedleyRestfulAcuteIllnessActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_findings_signs_general',
    'field_findings_signs_respiratory',
  ];

}
