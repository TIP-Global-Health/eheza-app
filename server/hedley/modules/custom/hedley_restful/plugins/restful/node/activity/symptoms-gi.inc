<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Symptoms GI'),
  'resource' => 'symptoms-gi',
  'name' => 'symptoms-gi',
  'entity_type' => 'node',
  'bundle' => 'symptoms_gi',
  'description' => t('Exports the Symptoms GI bundle.'),
  'class' => 'HedleyRestfulSymptomsGI',
  'authentication_types' => TRUE,
);
