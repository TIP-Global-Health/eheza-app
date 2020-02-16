<?php

/**
 * @file
 * Contains HedleyRestfulObstetricalExams.
 */

/**
 * Class HedleyRestfulObstetricalExams.
 */
class HedleyRestfulObstetricalExams extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_fundal_height',
    'field_fetal_presentation',
    'field_fetal_movement',
    'field_fetal_heart_rate',
    'field_c_section_scar',
  ];

}
