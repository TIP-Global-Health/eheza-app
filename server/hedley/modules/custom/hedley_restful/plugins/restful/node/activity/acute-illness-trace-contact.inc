<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Acute Illness Trace Contact'),
  'resource' => 'acute-illness-trace-contact',
  'name' => 'acute-illness-trace-contact',
  'entity_type' => 'node',
  'bundle' => 'acute_illness_trace_contact',
  'description' => t('Exports the AcuteIllnessTraceContact bundle.'),
  'class' => 'HedleyRestfulAcuteIllnessTraceContact',
  'authentication_types' => TRUE,
);
