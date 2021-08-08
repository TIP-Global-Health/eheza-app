<?php

/**
 * @file
 * Contains \HedleyMigrateNutritionEncounters.
 */

/**
 * Class HedleyMigrateNutritionEncounters.
 */
class HedleyMigrateNutritionEncounters extends HedleyMigrateIndividualEncounterBase {

  /**
   * {@inheritdoc}
   */
  protected $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'nutrition_encounter';

}
