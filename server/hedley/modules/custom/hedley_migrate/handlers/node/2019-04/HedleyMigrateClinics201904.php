<?php

/**
 * @file
 * Contains \HedleyMigrateClinics201904.
 */

/**
 * Class HedleyMigrateClinics201904.
 */
class HedleyMigrateClinics201904 extends HedleyMigrateBase {

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
    $this->dependencies = [
      'HedleyMigrateHealthCenters',
    ];

    $columns = [
      ['id', 'ID'],
      ['title', 'Clinic Name'],
      ['field_health_center', 'Health Center'],
    ];

    $source_file = $this->getMigrateDirectory() . '/csv/2019-04/clinics.csv';
    $options = ['header_rows' => 1];
    $this->source = new MigrateSourceCSV($source_file, $columns, $options);

    $key = array(
      'id' => [
        'type' => 'varchar',
        'length' => 255,
        'not null' => TRUE,
      ],
    );

    $this->destination = new MigrateDestinationNode($this->bundle);

    $this->map = new MigrateSQLMap($this->machineName, $key, MigrateDestinationNode::getKeySchema());

    $this->addFieldMapping('title', 'title');

    $this
      ->addFieldMapping('field_health_center', 'field_health_center')
      ->sourceMigration('HedleyMigrateHealthCenters');
  }

}
