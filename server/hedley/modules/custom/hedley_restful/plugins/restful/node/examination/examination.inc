<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Examination'),
  'resource' => 'examination',
  'name' => 'examination',
  'entity_type' => 'node',
  'bundle' => 'examination',
  'description' => t('Exports the Examination bundle.'),
  'class' => 'HedleyRestfulExamination',
  'authentication_types' => TRUE,
);
