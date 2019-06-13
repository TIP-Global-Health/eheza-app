<?php

/**
 * @file
 * Contains \HedleyMigratePeople.
 */

/**
 * Class HedleyMigratePeople.
 */
class HedleyMigratePeople extends HedleyMigrateBase {

  protected $entityType = 'node';
  protected $bundle = 'person';

  protected $csvColumns = [
    'id',
    'title',
    'field_gender',
    'field_birth_date',
    'field_clinic',
  ];

  protected $simpleMappings = [
    'field_gender',
  ];

  /**
   * HedleyMigrateClinics constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigrateClinics',
    ];

    $this
      ->addFieldMapping('field_birth_date', 'field_birth_date')
      ->callbacks([$this, 'dateProcess']);

    $this
      ->addFieldMapping('field_clinic', 'field_clinic')
      ->sourceMigration('HedleyMigrateClinics');
  }

  /**
   * Convert a date string to a timestamp.
   *
   * @param string $date
   *   A string containing a date.
   *
   * @return int
   *   A timestamp.
   */
  public function dateProcess($date) {
    // The source material uses several date formats.
    $trimmed = trim($date);

    if (empty($trimmed)) {
      return $trimmed;
    }

    if (preg_match('/^\\d\\d\\d\\d-\\d\\d-\\d\\d$/', $trimmed)) {
      return DateTime::createFromFormat('!Y-m-d', $trimmed)->getTimestamp();
    }

    throw new Exception("$date was not a recognized date format.");
  }

}
