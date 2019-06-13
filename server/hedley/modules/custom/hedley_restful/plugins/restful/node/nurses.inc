<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Nurses'),
  'resource' => 'nurses',
  'name' => 'nurses',
  'entity_type' => 'node',
  'bundle' => 'nurse',
  'description' => t('Exports the Nurses bundle.'),
  'class' => 'HedleyRestfulNurses',
  'authentication_types' => TRUE,
);
