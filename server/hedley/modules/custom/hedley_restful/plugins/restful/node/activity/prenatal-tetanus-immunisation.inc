<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Tetanus Immunisation'),
  'resource' => 'prenatal-tetanus-immunisation',
  'name' => 'prenatal-tetanus-immunisation',
  'entity_type' => 'node',
  'bundle' => 'prenatal_tetanus_immunisation',
  'description' => t('Exports the Prenatal Tetanus Immunisation bundle.'),
  'class' => 'HedleyRestfulPrenatalVaccination',
  'authentication_types' => TRUE,
);
