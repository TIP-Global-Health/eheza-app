<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Birth Plan'),
  'resource' => 'birth-plan',
  'name' => 'birth-plan',
  'entity_type' => 'node',
  'bundle' => 'birth_plan',
  'description' => t('Exports the Birth Plan bundle.'),
  'class' => 'HedleyRestfulBirthPlan',
  'authentication_types' => TRUE,
);
