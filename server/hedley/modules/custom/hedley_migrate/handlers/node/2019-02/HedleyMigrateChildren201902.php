<?php

/**
 * @file
 * Contains \HedleyMigrateChildren201902.
 */

/**
 * Class HedleyMigrateChildren201902.
 */
class HedleyMigrateChildren201902 extends HedleyMigrateBase {

  public $entityType = 'node';
  public $bundle = 'child';
  public $csvPrefix = '2019-02/';

  public $columns = [
    0 => ['id', 'id'],
    1 => ['mother', 'mother'],
    2 => ['first_name', 'first_name'],
    3 => ['middle_name', 'middle_name'],
    4 => ['second_name', 'second_name'],
    5 => ['birth_date', 'birth_date'],
    8 => ['gender', 'gender'],
    20 => ['cell', 'cell'],
    22 => ['health_center', 'health_center'],
  ];

  public $fields = [
    'title' => 'title',
  ];

  /**
   * HedleyMigrateChildren201902 constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigrateMothers_2019_02',
    ];

    $this->addFieldMapping('title', 'title');
    $this->addFieldMapping('field_gender', 'gender');

    $this
      ->addFieldMapping('field_date_birth', 'birth_date')
      ->callbacks([$this, 'dateProcess']);

    $this
      ->addFieldMapping('field_mother', 'mother')
      ->sourceMigration('HedleyMigrateMothers_2019_02');
  }

  /**
   * Calculate some values.
   */
  public function prepareRow($row) {
    if (parent::prepareRow($row) === FALSE) {
      return FALSE;
    }

    // We only want to import the children of mothers we have imported.
    // (We are selecting mothers based on their health center).
    $migratedMother = db_select('migrate_map_hedleymigratemothers_2019_02', 'm')
      ->condition('sourceid1', $row->mother)
      ->isNotNull('destid1')
      ->countQuery()
      ->execute()
      ->fetchField();

    if ($migratedMother == 0) {
      // We didn't migrate the mother.
      return FALSE;
    }

    // Calculate the child's name.
    $row->title = implode(' ', array_filter([
      trim($row->second_name),
      trim($row->first_name),
      trim($row->middle_name),
    ]));

    // Normalize Gender.
    if ($row->gender) {
      $row->gender = trim(strtolower($row->gender));

      if ($row->gender == 'm') {
        $row->gender = 'male';
      }

      if ($row->gender == 'f') {
        $row->gender = 'female';
      }

      if ($row->gender != 'female' && $row->gender != 'male') {
        throw new Exception("{$row->gender} is not a recognized gender");
      }
    }

    return TRUE;
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

    if (preg_match('/^\\d\\d-\\d\\d-\\d\\d$/', $trimmed)) {
      return DateTime::createFromFormat('!y-m-d', $trimmed)->getTimestamp();
    }

    if (preg_match('/^\\d\\d\\d\\d-\\d\\d-\\d\\d$/', $trimmed)) {
      return DateTime::createFromFormat('!Y-m-d', $trimmed)->getTimestamp();
    }

    if (preg_match('@^\\d\\d?/\\d\\d?/\\d\\d\\d\\d@', $trimmed)) {
      return DateTime::createFromFormat('!d/m/Y', $trimmed)->getTimestamp();
    }

    throw new Exception("$date was not a recognized date format.");
  }

}
