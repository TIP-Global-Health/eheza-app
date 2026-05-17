<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD Referral'),
  'resource' => 'ncd-referral',
  'name' => 'ncd-referral',
  'entity_type' => 'node',
  'bundle' => 'ncd_referral',
  'description' => t('Exports the NCD Referral bundle.'),
  'class' => 'HedleyRestfulNCDReferral',
  'authentication_types' => TRUE,
);
