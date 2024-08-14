<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('HIV Referral'),
  'resource' => 'hiv-referral',
  'name' => 'hiv-referral',
  'entity_type' => 'node',
  'bundle' => 'hiv_referral',
  'description' => t('Exports the HIV Referral bundle.'),
  'class' => 'HedleyRestfulHIVReferral',
  'authentication_types' => TRUE,
);
