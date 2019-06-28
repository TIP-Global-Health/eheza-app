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
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['medical_history'] = [
      'property' => 'field_medical_history',
    ];

    return $public_fields;
  }

}
