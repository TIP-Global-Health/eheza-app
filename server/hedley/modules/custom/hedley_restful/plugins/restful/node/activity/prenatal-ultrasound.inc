<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Ultrasound'),
  'resource' => 'prenatal-ultrasound',
  'name' => 'prenatal-ultrasound',
  'entity_type' => 'node',
  'bundle' => 'prenatal_ultrasound',
  'description' => t('Exports the Prenatal Ultrasound bundle.'),
  'class' => 'HedleyRestfulPrenatalUltrasound',
  'authentication_types' => TRUE,
);
