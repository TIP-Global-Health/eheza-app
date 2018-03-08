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

    $public_fields['z_score_age'] = [
      'property' => 'field_z_score_age',
    ];

    $public_fields['z_score_length'] = [
      'property' => 'field_z_score_length',
    ];

    return $public_fields;
  }

  /**
   * Return the type of the activity.
   *
   * @return string
   *   The type name.
   */
  protected static function getType() {
    return 'weight';
  }

}
