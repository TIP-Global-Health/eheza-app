<?php

/**
 * @file
 * Contains \HedleyMigrateMuac.
 */

/**
 * Class HedleyMigrateMuac.
 */
class HedleyMigrateMuac extends HedleyMigrateBase {

  public $entityType = 'node';
  public $bundle = 'muac';

  /**
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);
    $this->description = t('Import Muacs from the CSV.');
    $this->dependencies = [
      'HedleyMigrateChild',
    ];

    $column_names = [
      'title',
      'field_child',
      'field_date',
      'field_activity_status',
      'field_middle_circumference',
      'field_upper_circumference',
      'field_arm_circumference',
    ];

    $columns = [];
    foreach ($column_names as $column_name) {
      $columns[] = [$column_name, $column_name];
    }

    $source_file = $this->getMigrateDirectory() . '/csv/muac.csv';
    $options = array('header_rows' => 1);
    $this->source = new MigrateSourceCSV($source_file, $columns, $options);

    $key = array(
      'title' => array(
        'type' => 'varchar',
        'length' => 255,
        'not null' => TRUE,
      ),
    );

    $this->destination = new MigrateDestinationNode($this->bundle);

    $this->map = new MigrateSQLMap($this->machineName, $key, MigrateDestinationNode::getKeySchema());

    $simple_fields = drupal_map_assoc([
      'title',
      'field_activity_status',
      'field_middle_circumference',
      'field_upper_circumference',
      'field_arm_circumference',
    ]);

    $this->addSimpleMappings($simple_fields);

    $this->addFieldMapping('field_child', 'field_child')
      ->sourceMigration('HedleyMigrateChild');

    $this->addFieldMapping('field_date', 'field_date')
      ->callbacks([$this, 'dateProcess']);
  }

}
