<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD Family Planning'),
  'resource' => 'ncd-family-planning',
  'name' => 'ncd-family-planning',
  'entity_type' => 'node',
  'bundle' => 'ncd_family_planning',
  'description' => t('Exports the NCD Family Planning bundle.'),
  'class' => 'HedleyRestfulNCDFamilyPlanning',
  'authentication_types' => TRUE,
);
