<?php

/**
 * @file
 * Contains \HedleyMigratePmtctParticipants.
 */

/**
 * Class HedleyMigratePmtctParticipants.
 */
class HedleyMigratePmtctParticipants extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  protected $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'pmtct_participant';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'id',
        'field_person',
        'field_adult',
        'field_adult_activities',
        'field_expected',
        'field_clinic',
        'created',
      ]
    );
  }

  /**
   * {@inheritdoc}
   */
  protected function simpleMappings() {
    $mappings = parent::simpleMappings();

    return array_merge(
      $mappings, [
        'field_adult_activities',
      ]
    );
  }

  /**
   * HedleyMigratePmtctParticipants constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigratePeople',
      'HedleyMigrateClinics',
      'HedleyMigrateAttendances',
      'HedleyMigrateFamilyPlannings',
      'HedleyMigrateHeights',
      'HedleyMigrateMuacs',
      'HedleyMigrateNutritions',
      'HedleyMigrateWeights',
      'HedleyMigratePhotos',
    ];

    $this
      ->addFieldMapping('field_person', 'field_person')
      ->sourceMigration('HedleyMigratePeople');

    $this
      ->addFieldMapping('field_adult', 'field_adult')
      ->sourceMigration('HedleyMigratePeople');

    $this
      ->addFieldMapping('field_clinic', 'field_clinic')
      ->sourceMigration('HedleyMigrateClinics');

    $this
      ->addFieldMapping('field_expected', 'field_expected')
      ->callbacks([$this, 'date2Process']);
  }

}
