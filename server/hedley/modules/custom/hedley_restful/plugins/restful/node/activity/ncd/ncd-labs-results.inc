<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD Labs Results'),
  'resource' => 'ncd-labs-results',
  'name' => 'ncd-labs-results',
  'entity_type' => 'node',
  'bundle' => 'ncd_labs_results',
  'description' => t('Exports the NCD Labs Results bundle.'),
  'class' => 'HedleyRestfulNCDLabsResults',
  'authentication_types' => TRUE,
);
