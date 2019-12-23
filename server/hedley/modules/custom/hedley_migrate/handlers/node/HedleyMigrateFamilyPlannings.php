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
    'field_uuid',
    'field_session',
    'field_shards',
    'field_field_family_planning_signs',
  ];

  /**
   * {@inheritdoc}
   */
  protected $simpleMultipleMappings = [
    'field_field_family_planning_signs',
  ];

}
