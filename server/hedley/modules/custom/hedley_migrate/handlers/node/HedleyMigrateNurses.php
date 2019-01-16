<?php

/**
 * @file
 * Contains \HedleyMigrateNurses.
 */

/**
 * Class HedleyMigrateNurses.
 */
class HedleyMigrateNurses extends HedleyMigrateBase {

  public $entityType = 'node';
  public $bundle = 'nurse';

  /**
   * HedleyMigrateNurses constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);
    $this->description = t('Import nurses from the CSV.');
    $this->dependencies = [
      'HedleyMigrateClinics',
    ];

    $columns = [
      ['id', 'id'],
      ['title', 'title'],
      ['field_role', 'role'],
      ['field_clinics', 'clinics'],
    ];

    $source_file = $this->getMigrateDirectory() . '/csv/nurses.csv';
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
      ->addFieldMapping('field_role', 'field_role')
      ->separator('|');

    $this
      ->addFieldMapping('field_clinics', 'field_clinics')
      ->separator('|')
      ->sourceMigration('HedleyMigrateClinics');
  }

}
