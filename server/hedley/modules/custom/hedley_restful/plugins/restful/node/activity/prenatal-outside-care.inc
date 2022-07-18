<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Outside Care'),
  'resource' => 'prenatal-outside-care',
  'name' => 'prenatal-outside-care',
  'entity_type' => 'node',
  'bundle' => 'prenatal_outside_care',
  'description' => t('Exports the Prenatal Outside Care bundle.'),
  'class' => 'HedleyRestfulPrenatalOutsideCare',
  'authentication_types' => TRUE,
);
