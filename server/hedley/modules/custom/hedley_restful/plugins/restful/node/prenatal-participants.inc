<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Participants'),
  'resource' => 'prenatal-participants',
  'name' => 'prenatal-participants',
  'entity_type' => 'node',
  'bundle' => 'prenatal_participant',
  'description' => t('Exports the Prenatal Participant bundle.'),
  'class' => 'HedleyRestfulPrenatalParticipants',
  'authentication_types' => TRUE,
);
