<?php

/**
 * @file
 * Contains HedleyRestfulAcuteIllnessVitals.
 */

/**
 * Class HedleyRestfulAcuteIllnessVitals.
 */
class HedleyRestfulAcuteIllnessGU extends HedleyRestfulAcuteIllnessActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_coke_colored_urine_period',
    'field_frequent_urination_period',
    'field_dysuria_period',
    'field_c_b_d_urine_period',
    'field_abnormal_discharge_period',
    'field_genital_itching_period',
  ];

}
