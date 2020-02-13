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
  ];

  /**
   * {@inheritdoc}
   */
  protected $simpleMappings = [
    'field_encounter_type',
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
      ->callbacks([$this, 'dateProcess']);
  }

  /**
   * Convert a date string to a timestamp.
   *
   * @param string $date
   *   A string containing a date.
   *
   * @return array
   *   A start date.
   */
  public function dateProcess($date) {
    $trimmed = trim($date);

    if (empty($trimmed)) {
      return $trimmed;
    }

    if (preg_match('/^\\d\\d\\d\\d-\\d\\d-\\d\\d$/', $trimmed)) {
      $stamp = DateTime::createFromFormat('!Y-m-d', $trimmed)->getTimestamp();

      return [
        'value' => $stamp,
        'value2' => NULL,
      ];
    }

    throw new Exception("$date was not a recognized date format.");
  }

}
