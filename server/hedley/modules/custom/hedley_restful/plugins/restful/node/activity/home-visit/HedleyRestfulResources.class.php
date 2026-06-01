<?php

/**
 * @file
 * Contains HedleyRestfulResources.
 */

/**
 * Class HedleyRestfulResources.
 */
class HedleyRestfulResources extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_phase_recorded',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_resources',
  ];

}
