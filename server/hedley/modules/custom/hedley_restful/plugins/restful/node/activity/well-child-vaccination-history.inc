<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child VaccinationHistory'),
  'resource' => 'well-child-vaccination-history',
  'name' => 'well-child-vaccination-history',
  'entity_type' => 'node',
  'bundle' => 'well_child_vaccination_history',
  'description' => t('Exports the Well Child Vaccination History bundle.'),
  'class' => 'HedleyRestfulWellChildVaccinationHistory',
  'authentication_types' => TRUE,
);
