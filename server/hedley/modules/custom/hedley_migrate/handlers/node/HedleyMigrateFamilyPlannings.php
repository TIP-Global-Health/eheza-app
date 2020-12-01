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
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_family_planning_signs',
      ]
    );
  }

  /**
   * {@inheritdoc}
   */
  protected function simpleMultipleMappings() {
    $mappings = parent::simpleMultipleMappings();

    return array_merge(
      $mappings, [
        'field_family_planning_signs',
      ]
    );
  }

}
