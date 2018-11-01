<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Participants form'),
  'resource' => 'participants-form',
  'name' => 'participants-form',
  'entity_type' => 'node',
  'bundle' => 'participant_form',
  'description' => t('Exports the Participant Form bundle.'),
  'class' => 'HedleyRestfulParticipantsForm',
  'authentication_types' => TRUE,
);
