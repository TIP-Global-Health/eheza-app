<?php

/**
 * @file
 * Contains \HedleyMigrateClinics.
 */

/**
 * Class HedleyMigrateClinics.
 */
class HedleyMigrateClinics extends HedleyMigrateBase {

  public $entityType = 'node';
  public $bundle = 'clinic';

  /**
   * HedleyMigrateClinics constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);
    $this->description = t('Import clinics from the CSV.');
    $this->dependencies = [];

    $columns = array(
      ['name', t('Clinic Name')],
    );

    $source_file = $this->getMigrateDirectory() . '/csv/clinics.csv';
    $options = array('header_rows' => 1);
    $this->source = new MigrateSourceCSV($source_file, $columns, $options);

    $key = array(
      'name' => array(
        'type' => 'varchar',
        'length' => 255,
        'not null' => TRUE,
      ),
    );

    $this->destination = new MigrateDestinationNode($this->bundle);

    $this->map = new MigrateSQLMap($this->machineName, $key, MigrateDestinationNode::getKeySchema());

    $this->addFieldMapping('title', 'name');
  }

}
