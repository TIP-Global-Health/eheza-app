<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD Family Planning'),
  'resource' => 'ncd-random-blood-sugar-test',
  'name' => 'ncd-random-blood-sugar-test',
  'entity_type' => 'node',
  'bundle' => 'ncd_random_blood_sugar_test',
  'description' => t('Exports the NCD Family Planning bundle.'),
  'class' => 'HedleyRestfulNCDRandomBloodSugarTest',
  'authentication_types' => TRUE,
);
