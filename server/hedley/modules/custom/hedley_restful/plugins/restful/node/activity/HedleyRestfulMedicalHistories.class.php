<?php

/**
 * @file
 * Contains HedleyRestfulMedicalHistories.
 */

/**
 * Class HedleyRestfulMedicalHistories.
 */
class HedleyRestfulMedicalHistories extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multi_fields = [
    'field_medical_history',
  ];

}
