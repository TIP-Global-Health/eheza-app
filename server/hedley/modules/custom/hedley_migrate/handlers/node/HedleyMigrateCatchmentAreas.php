<?php

/**
 * @file
 * Contains \HedleyMigrateCatchmentAreas.
 */

/**
 * Class HedleyMigrateCatchmentAreas.
 */
class HedleyMigrateCatchmentAreas extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  public $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  public $bundle = 'catchment_area';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'id',
        'title',
        'created',
      ]
    );
  }

}
