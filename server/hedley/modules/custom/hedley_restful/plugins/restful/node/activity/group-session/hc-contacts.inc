<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('HC Contacts'),
  'resource' => 'hc-contacts',
  'name' => 'hc-contacts',
  'entity_type' => 'node',
  'bundle' => 'hc_contact',
  'description' => t('Exports the HC Contact bundle.'),
  'class' => 'HedleyRestfulHCContacts',
  'authentication_types' => TRUE,
);
