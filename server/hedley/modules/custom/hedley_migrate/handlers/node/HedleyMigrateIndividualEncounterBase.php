<?php

/**
 * @file
 * Contains \HedleyMigrateIndividualEncounterBase.
 */

/**
 * Class HedleyMigrateIndividualEncounterBase.
 */
abstract class HedleyMigrateIndividualEncounterBase extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'id',
        'field_individual_participant',
        'field_scheduled_date',
        'created',
      ]
    );
  }

  /**
   * HedleyMigrateIndividualEncounterBase constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigrateIndividualParticipants',
    ];

    $this
      ->addFieldMapping('field_scheduled_date', 'field_scheduled_date')
      ->callbacks([$this, 'date2Process']);

    $this
      ->addFieldMapping('field_individual_participant', 'field_individual_participant')
      ->sourceMigration('HedleyMigrateIndividualParticipants');
  }

}
