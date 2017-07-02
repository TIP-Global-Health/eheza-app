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
      ['avatar', t('User avatar')],
    );

    $source_file = $this->getMigrateDirectory() . '/csv/user.csv';
    $options = array('header_rows' => 1);
    $this->source = new MigrateSourceCSV($source_file, $columns, $options);

    $this->destination = new MigrateDestinationUser();

    $this->addFieldMapping('name', 'name');
    $this->addFieldMapping('mail', 'email');
    $this->addFieldMapping('pass', 'pass');

    // Map the file name to the title.
    $this->addFieldMapping('field_avatar', 'name')
      ->callbacks([$this, 'avatarProcess']);

    $this->addFieldMapping('field_avatar:file_replace')
      ->defaultValue(FILE_EXISTS_REPLACE);

    $this->addFieldMapping('field_avatar:source_dir')
      ->defaultValue($this->getMigrateDirectory() . '/images/');

    $this->addFieldMapping(('status'))
      ->defaultValue(1);

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

  /**
   * Assign role to the user.
   */
  function complete($entity, $row) {

    if (!$row->role) {
      return;
    }

    $names = explode(",", $row->role);

    foreach ($names as $name) {
      $role = user_role_load_by_name($name);
      $entity->roles[$role->rid] = $row->role;
    }

    user_save($entity);
  }

}
