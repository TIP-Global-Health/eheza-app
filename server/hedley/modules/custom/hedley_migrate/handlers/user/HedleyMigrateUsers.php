<?php

/**
 * @file
 * Contains \HedleyMigrateUsers.
 */

/**
 * Class HedleyMigrateUsers.
 */
class HedleyMigrateUsers extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  protected $entityType = 'user';

  /**
   * UnioMigrateUsers constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);
    $this->description = 'Import users from the CSV.';

    $columns = array(
      ['name', 'Username'],
      ['pass', 'User password'],
      ['email', 'User email'],
      ['role', 'User role'],
    );

    $source_file = $this->getMigrateDirectory() . '/csv/user.csv';
    $options = array('header_rows' => 1);
    $this->source = new MigrateSourceCSV($source_file, $columns, $options);

    $this->destination = new MigrateDestinationUser();
    $this->dependencies = [
      'HedleyMigrateClinics',
    ];

    $this->addFieldMapping('name', 'name');
    $this->addFieldMapping('mail', 'email');
    $this->addFieldMapping('pass', 'pass');
    $this->addFieldMapping('role_names', 'role');

    $this->addFieldMapping(('status'))
      ->defaultValue(NODE_PUBLISHED);

    $this->map = new MigrateSQLMap($this->machineName,
      array(
        'name' => array(
          'type' => 'varchar',
          'length' => 255,
          'not null' => TRUE,
        ),
      ),
      MigrateDestinationUser::getKeySchema()
    );
  }

}
