<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Symptoms General'),
  'resource' => 'symptoms-general',
  'name' => 'symptoms-general',
  'entity_type' => 'node',
  'bundle' => 'symptoms_general',
  'description' => t('Exports the Symptoms General bundle.'),
  'class' => 'HedleyRestfulSymptomsGeneral',
  'authentication_types' => TRUE,
);
