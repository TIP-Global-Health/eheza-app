<?php

/**
 * @file
 * Contains HedleyRestfulNutritionFollowUp.
 */

/**
 * Class HedleyRestfulNutritionFollowUp.
 */
class HedleyRestfulNutritionFollowUp extends HedleyRestfulNutritionActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_contact_date',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_follow_up_options',
    'field_nutrition_assesment',
    'field_nutrition_signs',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_contact_date',
  ];

}
