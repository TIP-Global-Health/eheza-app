<?php
/**
 * @file
 * Contains HedleyRestfulBirthPlan.
 */

/**
 * Class HedleyRestfulBirthPlan.
 */

class HedleyRestfulBirthPlan extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_birth_plan_signs',
    'field_family_planning_signs',
  ];

}
