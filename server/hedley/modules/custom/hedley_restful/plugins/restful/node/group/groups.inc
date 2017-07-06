<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Groups'),
  'resource' => 'groups',
  'name' => 'group',
  'entity_type' => 'node',
  'bundle' => 'group',
  'description' => t('Exports the Group bundle.'),
  'class' => 'HedleyRestfulGroups',
  'authentication_types' => TRUE,
);
