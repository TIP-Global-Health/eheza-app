<?php

/**
 * @file
 * Contains \HedleyMigrateHealthCenters.
 */

/**
 * Class HedleyMigrateHealthCenters.
 */
class HedleyMigrateHealthCenters extends HedleyMigrateBase {

  public $entityType = 'node';
  public $bundle = 'health_center';

  /**
   * HedleyMigrateHealthCenters constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);
    $this->description = t('Import health centers from the CSV.');
    $this->dependencies = [
      'HedleyMigrateCatchmentAreas',
    ];

    $columns = [
      ['id', t('id')],
      ['title', t('title')],
      ['field_catchment_area', t('Catchment Area')],
    ];

    $source_file = $this->getMigrateDirectory() . '/csv/health-centers.csv';
    $options = ['header_rows' => 1];
    $this->source = new MigrateSourceCSV($source_file, $columns, $options);

    $key = [
      'id' => [
        'type' => 'varchar',
        'length' => 255,
        'not null' => TRUE,
      ],
    ];

    $this->destination = new MigrateDestinationNode($this->bundle);

    $this->map = new MigrateSQLMap($this->machineName, $key, MigrateDestinationNode::getKeySchema());

    $this->addFieldMapping('title', 'title');

    $this
      ->addFieldMapping('field_catchment_area', 'field_catchment_area')
      ->sourceMigration('HedleyMigrateCatchmentAreas');
  }

}
