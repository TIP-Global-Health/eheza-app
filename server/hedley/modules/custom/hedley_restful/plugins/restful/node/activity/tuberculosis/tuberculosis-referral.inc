<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Tuberculosis Referral'),
  'resource' => 'tuberculosis-referral',
  'name' => 'tuberculosis-referral',
  'entity_type' => 'node',
  'bundle' => 'tuberculosis_referral',
  'description' => t('Exports the Tuberculosis Referral bundle.'),
  'class' => 'HedleyRestfulTuberculosisReferral',
  'authentication_types' => TRUE,
);
