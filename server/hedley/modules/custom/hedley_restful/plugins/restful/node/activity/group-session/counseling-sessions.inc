<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Counseling Sessions'),
  'resource' => 'counseling-sessions',
  'name' => 'counseling-sessions',
  'entity_type' => 'node',
  'bundle' => 'counseling_session',
  'description' => t('Exports the Counseling Session bundle.'),
  'class' => 'HedleyRestfulCounselingSessions',
  'authentication_types' => TRUE,
);
