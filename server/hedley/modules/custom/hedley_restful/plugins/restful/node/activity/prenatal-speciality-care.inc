<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Speciality Care'),
  'resource' => 'prenatal-speciality-care',
  'name' => 'prenatal-speciality-care',
  'entity_type' => 'node',
  'bundle' => 'prenatal_speciality_care',
  'description' => t('Exports the Prenatal Speciality Care bundle.'),
  'class' => 'HedleyRestfulPrenatalSpecialityCare',
  'authentication_types' => TRUE,
);
