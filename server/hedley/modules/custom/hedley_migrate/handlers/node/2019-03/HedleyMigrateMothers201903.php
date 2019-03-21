<?php

/**
 * @file
 * Contains \HedleyMigrateMothers201903.
 */

/**
 * Class HedleyMigrateMothers201903.
 */
class HedleyMigrateMothers201903 extends HedleyMigrateBase {

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

    // We need the clinics from the previous migration.
    $this->dependencies = [
      'HedleyMigrateClinics_2019_02',
    ];

    // For now, we're not using all columns. We'll eventually modify this and
    // do a `drush mi --update` when we have new fields in our data model to
    // fill.
    $columns = [
      1 => ['first_name', 'first_name'],
      2 => ['second_name', 'second_name'],
      3 => ['birth_date', 'birth_date'],
      5 => ['national_id', 'national_id'],
      7 => ['ubudehe', 'ubudehe'],
      8 => ['education', 'education'],
      18 => ['cell', 'cell'],
      20 => ['health_center', 'health_center'],
    ];

    // Will be added in prepareRow.
    $fields = [
      'title' => 'title',
      'clinic' => 'clinic',
    ];

    $source_file = $this->getMigrateDirectory() . '/csv/2019-03/mothers.csv';
    $options = ['header_rows' => 1];
    $this->source = new MigrateSourceCSV($source_file, $columns, $options, $fields);

    // In this batch, the national ID is provided and unique, so we'll use that
    // to track the source rows.
    $key = array(
      'national_id' => [
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
        $row->education = HEDLEY_PATIENT_EDUCATION_PRIMARY;
      }
      elseif ($row->education === 'NTABWOYIZE') {
        // Means "no education".
        $row->education = HEDLEY_PATIENT_EDUCATION_NONE;
      }
      elseif (preg_match('/^P[1-8]$/', $row->education)) {
        $row->education = HEDLEY_PATIENT_EDUCATION_PRIMARY;
      }
      elseif (preg_match('/^S[1-6]$/', $row->education)) {
        $row->education = HEDLEY_PATIENT_EDUCATION_SECONDARY;
      }
      else {
        throw new Exception("{$row->education} is not a recognized education level.");
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

    // Some dates are year-only ... we'll make those Jan. 1.
    if (preg_match('/^\\d\\d\\d\\d$/', $trimmed)) {
      $trimmed = "$trimmed-01-01";
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
