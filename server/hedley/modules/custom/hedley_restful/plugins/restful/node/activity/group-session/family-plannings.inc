<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Family Plannings'),
  'resource' => 'family-plannings',
  'name' => 'family-plannings',
  'entity_type' => 'node',
  'bundle' => 'family_planning',
  'description' => t('Exports the Family Planning bundle.'),
  'class' => 'HedleyRestfulFamilyPlannings',
  'authentication_types' => TRUE,
);
