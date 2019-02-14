<?php

/**
 * @file
 * Contains \HedleyMigrateMothers201902.
 */

/**
 * Class HedleyMigrateMothers201902.
 */
class HedleyMigrateMothers201902 extends HedleyMigrateBase {

  public $entityType = 'node';
  public $bundle = 'mother';

  /**
   * HedleyMigrateClinics constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);
    $this->description = t('Import mothers from the CSV.');
    $this->dependencies = [
      'HedleyMigrateClinics_2019_02',
    ];

    // For now, we're not using all columns. We'll eventually modify this and
    // do a `drush mi --update` when we have new fields in our data model to
    // fill.
    $columns = [
      0 => ['id', 'id'],
      1 => ['first_name', 'first_name'],
      2 => ['middle_name', 'middle_name'],
      3 => ['second_name', 'second_name'],
      4 => ['birth_date', 'birth_date'],
      8 => ['ubudehe', 'ubudehe'],
      9 => ['education', 'education'],
      19 => ['cell', 'cell'],
      21 => ['health_center', 'health_center'],
    ];

    // Will be added in prepareRow.
    $fields = [
      'title' => 'title',
      'clinic' => 'clinic',
    ];

    $source_file = $this->getMigrateDirectory() . '/csv/2019-02/mothers.csv';
    $options = ['header_rows' => 1];
    $this->source = new MigrateSourceCSV($source_file, $columns, $options, $fields);

    $key = array(
      'id' => [
        'type' => 'varchar',
        'length' => 255,
        'not null' => TRUE,
      ],
    );

    $this->destination = new MigrateDestinationNode($this->bundle);

    $this->map = new MigrateSQLMap($this->machineName, $key, MigrateDestinationNode::getKeySchema());

    $this
      ->addFieldMapping('uid', 'author')
      ->defaultValue(1);

    $this->addFieldMapping('title', 'title');
    $this->addFieldMapping('field_education_level', 'education');
    $this->addFieldMapping('field_ubudehe', 'ubudehe');

    $this
      ->addFieldMapping('field_date_birth', 'birth_date')
      ->callbacks([$this, 'dateProcess']);

    $this
      ->addFieldMapping('field_clinic', 'clinic')
      ->sourceMigration('HedleyMigrateClinics_2019_02');
  }

  /**
   * Calculate some values.
   */
  public function prepareRow($row) {
    if (parent::prepareRow($row) === FALSE) {
      return FALSE;
    }

    $health_center = strtoupper($row->health_center);

    // Calculate which clinic to put the mother into.
    if ($health_center === 'COKO') {
      // The COKO mothers are all in one clinic.
      $row->clinic = 'COKO';
    }
    elseif ($health_center === 'RWANKUBA') {
      // For the RWANKUBA mothers, we're dividing into clinics according to
      // cell.
      $row->clinic = $row->cell;
    }
    else {
      // For now, we're only importing for some health centers.
      return FALSE;
    }

    // Calculate the mother's name.
    $row->title = implode(' ', array_filter([
      trim($row->second_name),
      trim($row->first_name),
      trim($row->middle_name),
    ]));

    // Education.
    if ($row->education) {
      if ($row->education === 'Primary') {
        $row->education = 1;
      }
      elseif ($row->education === 'NTABWOYIZE') {
        // Means "no education".
        $row->education = 0;
      }
      elseif (preg_match('/^P[1-8]$/', $row->education)) {
        $row->education = 1;
      }
      elseif (preg_match('/^S[1-6]$/', $row->education)) {
        $row->education = 3;
      }
      else {
        $this->queueMessage("{$row->education} is not a recognized education level.");
      }
    }
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

    $this->queueMessage("$date was not a recognized date format.");
    return "";
  }

}
