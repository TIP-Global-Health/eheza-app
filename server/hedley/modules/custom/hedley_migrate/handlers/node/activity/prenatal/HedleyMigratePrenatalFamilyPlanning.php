<?php

/**
 * @file
 * Contains \HedleyMigratePrenatalFamilyPlanning.
 */

/**
 * Class HedleyMigratePrenatalFamilyPlanning.
 */
class HedleyMigratePrenatalFamilyPlanning extends HedleyMigrateGroupMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'prenatal_family_planning';

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
