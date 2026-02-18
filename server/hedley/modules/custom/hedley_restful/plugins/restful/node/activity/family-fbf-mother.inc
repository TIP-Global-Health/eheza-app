<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Family FBF Mother'),
  'resource' => 'family-fbf-mother',
  'name' => 'family-fbf-mother',
  'entity_type' => 'node',
  'bundle' => 'family_fbf_mother',
  'description' => t('Exports the Family FBF Mother bundle.'),
  'class' => 'HedleyRestfulFamilyFBFMother',
  'authentication_types' => TRUE,
);
