<?php

/**
 * @file
 * Contains \HedleyMigrateIndividualParticipants.
 */

/**
 * Class HedleyMigrateIndividualParticipants.
 */
class HedleyMigrateIndividualParticipants extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  protected $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'individual_participant';

  /**
   * {@inheritdoc}
   */
  protected $csvColumns = [
    'id',
    'field_person',
    'field_expected',
    'field_encounter_type',
    'field_date_concluded',
    'field_outcome',
    'field_outcome_location',
    'field_expected_date_concluded',
  ];

  /**
   * {@inheritdoc}
   */
  protected $simpleMappings = [
    'field_encounter_type',
    'field_outcome',
    'field_outcome_location',
  ];

  /**
   * HedleyMigrateRelationships constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigratePeople',
    ];

    $this
      ->addFieldMapping('field_person', 'field_person')
      ->sourceMigration('HedleyMigratePeople');

    $this
      ->addFieldMapping('field_expected', 'field_expected')
      ->callbacks([$this, 'date2Process']);

    $this
      ->addFieldMapping('field_date_concluded', 'field_date_concluded')
      ->callbacks([$this, 'dateProcess']);

    $this
      ->addFieldMapping('field_expected_date_concluded', 'field_expected_date_concluded')
      ->callbacks([$this, 'dateProcess']);
  }

}
