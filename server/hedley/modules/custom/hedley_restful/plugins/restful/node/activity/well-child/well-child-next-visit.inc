<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child Next Visit'),
  'resource' => 'well-child-next-visit',
  'name' => 'well-child-next-visit',
  'entity_type' => 'node',
  'bundle' => 'well_child_next_visit',
  'description' => t('Exports the Well Child Next Visit bundle.'),
  'class' => 'HedleyRestfulWellChildNextVisit',
  'authentication_types' => TRUE,
);
