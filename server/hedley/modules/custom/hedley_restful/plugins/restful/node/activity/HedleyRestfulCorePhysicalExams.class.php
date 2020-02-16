<?php

/**
 * @file
 * Contains HedleyRestfulCorePhysicalExames.
 */

/**
 * Class HedleyRestfulCorePhysicalExams.
 */
class HedleyRestfulCorePhysicalExams extends HedleyRestfulPrenatalActivityBase {

  protected $fields = [
    'field_head_hair',
    'field_eyes',
    'field_heart',
    'field_heart_murmur',
  ];

  protected $multi_fields = [
    'field_neck',
    'field_lungs',
    'field_abdomen',
    'field_hands',
    'field_legs',
  ];

}
