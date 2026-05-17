<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Appointment Confirmation'),
  'resource' => 'appointment-confirmation',
  'name' => 'appointment-confirmation',
  'entity_type' => 'node',
  'bundle' => 'appointment_confirmation',
  'description' => t('Exports the AppointmentConfirmation bundle.'),
  'class' => 'HedleyRestfulAppointmentConfirmation',
  'authentication_types' => TRUE,
);
