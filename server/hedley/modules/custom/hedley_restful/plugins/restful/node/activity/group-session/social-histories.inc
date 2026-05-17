<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Social Histories'),
  'resource' => 'social-histories',
  'name' => 'social-histories',
  'entity_type' => 'node',
  'bundle' => 'social_history',
  'description' => t('Exports the Social History bundle.'),
  'class' => 'HedleyRestfulSocialHistories',
  'authentication_types' => TRUE,
);
