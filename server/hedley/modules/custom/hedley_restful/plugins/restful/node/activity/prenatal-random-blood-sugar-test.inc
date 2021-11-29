<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Random Blood Sugar Test'),
  'resource' => 'prenatal-random-blood-sugar-test',
  'name' => 'prenatal-random-blood-sugar-test',
  'entity_type' => 'node',
  'bundle' => 'prenatal_random_blood_sugar_test',
  'description' => t('Exports the Prenatal Random Blood Sugar Test bundle.'),
  'class' => 'HedleyRestfulPrenatalRandomBloodSugarTest',
  'authentication_types' => TRUE,
);
