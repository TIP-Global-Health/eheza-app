<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Family FBF Child'),
  'resource' => 'family-fbf-child',
  'name' => 'family-fbf-child',
  'entity_type' => 'node',
  'bundle' => 'family_fbf_child',
  'description' => t('Exports the Family FBF Child bundle.'),
  'class' => 'HedleyRestfulFamilyFBFChild',
  'authentication_types' => TRUE,
);
