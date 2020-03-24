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
  protected $csvColumns = [
    'id',
    'field_person',
    'field_date_measured',
    'field_nurse',
    'field_session',
    'field_family_planning_signs',
  ];

  /**
   * {@inheritdoc}
   */
  protected $simpleMultipleMappings = [
    'field_family_planning_signs',
  ];

}
