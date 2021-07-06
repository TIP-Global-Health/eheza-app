<?php

/**
 * @file
 * Contains HedleyRestfulWellChildHeadCircumference.
 */

/**
 * Class HedleyRestfulWellChildHeadCircumference.
 */
class HedleyRestfulWellChildHeadCircumference extends HedleyRestfulWellChildActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_head_circumference',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_measurement_notes',
  ];


}
