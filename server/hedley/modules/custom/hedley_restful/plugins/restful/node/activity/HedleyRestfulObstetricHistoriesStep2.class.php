<?php

/**
 * @file
 * Contains HedleyRestfulObstetricHistoriesStep2.
 */

/**
 * Class HedleyRestfulObstetricHistoriesStep2.
 */
class HedleyRestfulObstetricHistoriesStep2 extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_c_sections',
    'field_c_section_reason',
    'field_previous_delivery_period',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multi_fields = [
    'field_obstetric_history',
    'field_previous_delivery',
  ];

}
