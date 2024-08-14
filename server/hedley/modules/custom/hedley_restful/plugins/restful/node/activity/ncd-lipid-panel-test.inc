<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD Lipid Panel'),
  'resource' => 'ncd-lipid-panel-test',
  'name' => 'ncd-lipid-panel-test',
  'entity_type' => 'node',
  'bundle' => 'ncd_lipid_panel_test',
  'description' => t('Exports the NCD Lipid Panel bundle.'),
  'class' => 'HedleyRestfulNCDLipidPanelTest',
  'authentication_types' => TRUE,
);
