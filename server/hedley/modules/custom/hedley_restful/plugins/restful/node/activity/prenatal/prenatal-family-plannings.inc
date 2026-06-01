<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Family Plannings'),
  'resource' => 'prenatal-family-plannings',
  'name' => 'prenatal-family-plannings',
  'entity_type' => 'node',
  'bundle' => 'prenatal_family_planning',
  'description' => t('Exports the Prenatal Family Planning bundle.'),
  'class' => 'HedleyRestfulPrenatalFamilyPlannings',
  'authentication_types' => TRUE,
);
