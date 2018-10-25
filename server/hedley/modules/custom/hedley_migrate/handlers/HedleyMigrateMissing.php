<?php

/**
 * @file
 * Contains \HedleyMigrateMissing.
 */

/**
 * Class HedleyMigrateMissing.
 *
 * Helps migrate some missing data converted from Excel spreadsheets.
 */
abstract class HedleyMigrateMissing extends Migration {

  protected $entityType = 'node';
  protected $bundle = NULL;

  protected $csvColumns = [
    'id',
    'field_session',
    'field_date_measured',
    'field_child',
    'field_weight',
    'field_height',
    'field_muac',
  ];

  protected $simpleMappings = [];

  protected $simpleMultipleMappings = [];

  /**
   * HedleyMigrateBase constructor.
   */
  public function __construct($arguments) {
    parent::__construct($arguments);
    $destination_classes = [
      'node' => 'MigrateDestinationNode',
      'taxonomy_term' => 'MigrateDestinationTerm',
    ];

    // Add default settings, only for nodes, terms and multifields.
    if (empty($destination_classes[$this->entityType])) {
      return;
    }

    $this->description = t('Import Missing @bundle.', ['@bundle' => $this->bundle]);

    $source_file = $this->getMigrateDirectory() . '/csv/missing.csv';

    $columns = [];
    foreach ($this->csvColumns as $column_name) {
      $columns[] = [$column_name, $column_name];
    }
    $this->source = new MigrateSourceCSV($source_file, $columns, ['header_rows' => 1]);

    $destination_class = $destination_classes[$this->entityType];
    $this->destination = new $destination_class($this->bundle);

    $key = [
      'id' => [
        'type' => 'varchar',
        'length' => 255,
        'not null' => TRUE,
      ],
    ];

    $this->map = new MigrateSQLMap($this->machineName, $key, $this->destination->getKeySchema($this->entityType));

    // These are common to all.
    $this->addSimpleMappings([
      'field_session',
      'field_child',
    ]);

    // Common to all, and requires some processing.
    $this->addDateFields(['field_date_measured']);

    // Add specific mappings.
    if ($this->simpleMappings) {
      $this->addSimpleMappings($this->simpleMappings);
    }

    foreach ($this->simpleMultipleMappings as $field) {
      $this
        ->addFieldMapping($field, $field)
        ->separator('|');
    }

    if ($this->entityType == 'node') {
      // Set the first user as the author.
      $this
        ->addFieldMapping('uid', 'author')
        ->defaultValue(1);
      // Map the title field to the default title.
      if (in_array('title', $this->csvColumns)) {
        $this->addFieldMapping('title', 'title');
      }
      elseif (in_array('title_field', $this->csvColumns)) {
        $this->addFieldMapping('title', 'title_field');
      }
    }
    elseif ($this->entityType == 'taxonomy_term') {
      // Map the translated name field to the default term name.
      if (in_array('name', $this->csvColumns)) {
        $this->addFieldMapping('name', 'name');
      }
    }

  }

  /**
   * Returns the migrate directory.
   *
   * @return string
   *   The migrate directory.
   */
  protected function getMigrateDirectory() {
    return variable_get('hedley_migrate_directory', FALSE) ? variable_get('hedley_migrate_directory') : drupal_get_path('module', 'hedley_migrate');
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
    return strtotime($date);
  }

  /**
   * Add date fields.
   *
   * @param array $field_names
   *   The date related field names.
   */
  public function addDateFields(array $field_names) {
    foreach ($field_names as $field_name) {
      $this->addFieldMapping($field_name, $field_name)
        ->callbacks([$this, 'dateProcess']);
    }
  }

}
