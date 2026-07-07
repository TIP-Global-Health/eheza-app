<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD Social History'),
  'resource' => 'ncd-social-history',
  'name' => 'ncd-social-history',
  'entity_type' => 'node',
  'bundle' => 'ncd_social_history',
  'description' => t('Exports the NCD Social History bundle.'),
  'class' => 'HedleyRestfulNCDSocialHistory',
  'authentication_types' => TRUE,
);
