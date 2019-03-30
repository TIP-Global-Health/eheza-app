<?php

/**
 * @file
 * Contains \HedleyMigratePmtctParticipants.
 */

/**
 * Class HedleyMigratePmtctParticipants.
 */
class HedleyMigratePmtctParticipants extends HedleyMigrateBase {

  protected $entityType = 'node';
  protected $bundle = 'pmtct_participant';

  protected $csvColumns = [
    'id',
    'field_person',
    'field_adult',
    'field_adult_activities',
    'field_expected',
    'field_clinic',
  ];

  protected $simpleMappings = [
    'field_adult_activities',
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
      'HedleyMigrateClinics',
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
