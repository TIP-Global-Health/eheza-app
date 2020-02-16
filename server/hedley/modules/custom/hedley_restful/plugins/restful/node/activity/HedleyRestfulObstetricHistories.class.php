<?php

/**
 * @file
 * Contains HedleyRestfulObstetricHistories.
 */

/**
 * Class HedleyRestfulObstetricHistories.
 */
class HedleyRestfulObstetricHistories extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_currently_pregnant',
    'field_term_pregnancy',
    'field_preterm_pregnancy',
    'field_stillbirths_at_term',
    'field_stillbirths_preterm',
    'field_abortions',
    'field_live_children',
  ];

}
