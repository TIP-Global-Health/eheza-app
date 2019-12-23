<?php

/**
 * @file
 * Contains \HedleyMigrateFamilyPlannings.
 */

/**
 * Class HedleyMigrateFamilyPlannings.
 */
class HedleyMigrateFamilyPlannings extends HedleyMigrateMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'family_planning';

  /**
   * {@inheritdoc}
   */
  protected $simpleMultipleMappings = [
    'field_field_family_planning_signs',
  ];

}
