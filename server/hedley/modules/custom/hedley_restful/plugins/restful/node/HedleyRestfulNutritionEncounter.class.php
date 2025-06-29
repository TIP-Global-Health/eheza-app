<?php

/**
 * @file
 * Contains \HedleyRestfulNutritionEncounter.
 */

/**
 * Class HedleyRestfulNutritionEncounter.
 */
class HedleyRestfulNutritionEncounter extends HedleyRestfulIndividualEncounter {

  /**
   * A list of fields that are assigned single value.
   *
   * @var array
   */
  protected $fields = [
    'field_nutrition_encounter_type',
  ];

  /**
   * A list of fields that are assigned multiple values.
   *
   * @var array
   */
  protected $multiFields = [
    'field_skipped_forms',
  ];

}
