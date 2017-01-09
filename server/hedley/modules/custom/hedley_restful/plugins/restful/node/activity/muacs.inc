<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Muacs'),
  'resource' => 'muacs',
  'name' => 'muacs',
  'entity_type' => 'node',
  'bundle' => 'muac',
  'description' => t('Exports the MUAC bundle.'),
  'class' => 'HedleyRestfulMuacs',
  'authentication_types' => TRUE,
);
