<?php

/**
 * @file
 * Contains \HedleyMigrateUsers.
 */

/**
 * Class HedleyMigrateUsers.
 */
class HedleyMigrateUsers extends HedleyMigrateBase {

  public $entityType = 'user';

  /**
   * UnioMigrateUsers constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);
    $this->description = t('Import users from the CSV.');

    $columns = array(
      ['name', t('Username')],
      ['pass', t('User password')],
      ['email', t('User email')],
      ['role', t('User role')],
      ['clinics', t('Clinics')],
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

    $this->addFieldMapping(('status'))
      ->defaultValue(1);

    $this->addFieldMapping('role_names', 'role');

    $this
      ->addFieldMapping('field_clinics', 'clinics')
      ->separator('|')
      ->sourceMigration('HedleyMigrateClinics');

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
