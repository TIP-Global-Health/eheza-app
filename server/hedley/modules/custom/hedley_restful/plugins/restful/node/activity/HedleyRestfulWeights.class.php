<?php

/**
 * @file
 * Contains HedleyRestfulWeights.
 */

/**
 * Class HedleyRestfulWeights.
 */
class HedleyRestfulWeights extends HedleyRestfulChildActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['weight'] = [
      'property' => 'field_weight',
    ];

    $public_fields['bmi'] = [
      'property' => 'field_bmi',
    ];

    $public_fields['zscore_age'] = [
      'property' => 'field_zscore_age',
    ];

    $public_fields['zscore_length'] = [
      'property' => 'field_zscore_length',
    ];

    $public_fields['zscore_bmi'] = [
      'property' => 'field_zscore_bmi',
    ];

    return $public_fields;
  }

}
