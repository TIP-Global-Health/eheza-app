<?php

/**
 * @file
 * Contains HedleyRestfulIsolations.
 */

/**
 * Class HedleyRestfulIsolations.
 */
class HedleyRestfulIsolations extends HedleyRestfulAcuteIllnessActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_isolation',
    'field_reason_for_not_isolating',
  ];

}
