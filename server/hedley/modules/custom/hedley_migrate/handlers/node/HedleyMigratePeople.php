<?php

/**
 * @file
 * Contains \HedleyMigratePeople.
 */

/**
 * Class HedleyMigratePeople.
 */
class HedleyMigratePeople extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  protected $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'person';

  /**
   * {@inheritdoc}
   */
  protected $csvColumns = [
    'id',
    'field_first_name',
    'field_second_name',
    'field_gender',
    'field_birth_date',
    'field_health_center',
  ];

  /**
   * {@inheritdoc}
   */
  protected $simpleMappings = [
    'field_first_name',
    'field_second_name',
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
      'HedleyMigrateHealthCenters',
    ];

    $this
      ->addFieldMapping('field_birth_date', 'field_birth_date')
      ->callbacks([$this, 'dateProcess']);

    $this
      ->addFieldMapping('field_health_center', 'field_health_center')
      ->sourceMigration('HedleyMigrateHealthCenters');
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
      return DateTime::createFromFormat('!Y-m-d', $trimmed, new DateTimeZone("UTC"))->getTimestamp();
    }

    throw new Exception("$date was not a recognized date format.");
  }

}
