<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Symptoms Respiratory'),
  'resource' => 'symptoms-respiratory',
  'name' => 'symptoms-respiratory',
  'entity_type' => 'node',
  'bundle' => 'symptoms_respiratory',
  'description' => t('Exports the Symptoms Respiratory bundle.'),
  'class' => 'HedleyRestfulSymptomsRespiratory',
  'authentication_types' => TRUE,
);
