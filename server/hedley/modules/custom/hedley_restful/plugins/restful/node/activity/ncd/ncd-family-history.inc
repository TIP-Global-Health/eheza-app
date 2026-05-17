<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD Family History'),
  'resource' => 'ncd-family-history',
  'name' => 'ncd-family-history',
  'entity_type' => 'node',
  'bundle' => 'ncd_family_history',
  'description' => t('Exports the NCD Family History bundle.'),
  'class' => 'HedleyRestfulNCDFamilyHistory',
  'authentication_types' => TRUE,
);
