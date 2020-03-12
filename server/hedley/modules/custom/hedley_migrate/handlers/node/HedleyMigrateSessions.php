<?php

/**
 * @file
 * Contains \HedleyMigrateSessions.
 */

/**
 * Class HedleyMigrateSessions.
 */
class HedleyMigrateSessions extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  protected $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'session';

  /**
   * {@inheritdoc}
   */
  protected $csvColumns = [
    'id',
    'field_clinic',
    'field_scheduled_date',
  ];

  /**
   * HedleyMigrateSessions constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigrateClinics',
    ];

    $this
      ->addFieldMapping('field_scheduled_date', 'field_scheduled_date')
      ->callbacks([$this, 'date2Process']);

    $this
      ->addFieldMapping('field_clinic', 'field_clinic')
      ->sourceMigration('HedleyMigrateClinics');
  }

}
