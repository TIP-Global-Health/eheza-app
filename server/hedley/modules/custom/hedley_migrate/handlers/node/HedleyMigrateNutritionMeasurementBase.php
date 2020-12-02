<?php

/**
 * @file
 * Contains \HedleyMigrateNutritionMeasurementBase.
 */

/**
 * Class HedleyMigrateNutritionMeasurementBase.
 */
abstract class HedleyMigrateNutritionMeasurementBase extends HedleyMigrateMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_nutrition_encounter',
      ]
    );
  }

  /**
   * HedleyMigrateNutritionMeasurementBase constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigrateNutritionEncounters',
    ];

    $this
      ->addFieldMapping('field_nutrition_encounter', 'field_nutrition_encounter')
      ->sourceMigration('HedleyMigrateNutritionEncounters');
  }

}
