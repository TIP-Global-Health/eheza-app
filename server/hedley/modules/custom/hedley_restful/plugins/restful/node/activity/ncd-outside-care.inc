<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD Outside Care'),
  'resource' => 'ncd-outside-care',
  'name' => 'ncd-outside-care',
  'entity_type' => 'node',
  'bundle' => 'ncd_outside_care',
  'description' => t('Exports the NCD Outside Care bundle.'),
  'class' => 'HedleyRestfulNCDOutsideCare',
  'authentication_types' => TRUE,
);
