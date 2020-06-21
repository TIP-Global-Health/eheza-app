<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Acute Findings'),
  'resource' => 'acute-findings',
  'name' => 'acute-findings',
  'entity_type' => 'node',
  'bundle' => 'acute_findings',
  'description' => t('Exports the Acute Findings bundle.'),
  'class' => 'HedleyRestfulAcuteFindings',
  'authentication_types' => TRUE,
);
