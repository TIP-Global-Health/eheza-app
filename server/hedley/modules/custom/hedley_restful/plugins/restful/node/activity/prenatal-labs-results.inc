<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Labs Results'),
  'resource' => 'prenatal-labs-results',
  'name' => 'prenatal-labs-results',
  'entity_type' => 'node',
  'bundle' => 'prenatal_labs_results',
  'description' => t('Exports the Prenatal Labs Results bundle.'),
  'class' => 'HedleyRestfulPrenatalLabsResults',
  'authentication_types' => TRUE,
);
