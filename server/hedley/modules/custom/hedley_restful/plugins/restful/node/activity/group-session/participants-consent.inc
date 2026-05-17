<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Participants consent'),
  'resource' => 'participants-consent',
  'name' => 'participants-consent',
  'entity_type' => 'node',
  'bundle' => 'participant_consent',
  'description' => t('Exports the Participant Consent bundle.'),
  'class' => 'HedleyRestfulParticipantsConsent',
  'authentication_types' => TRUE,
);
