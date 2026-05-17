<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child Muac'),
  'resource' => 'well-child-head-circumference',
  'name' => 'well-child-head-circumference',
  'entity_type' => 'node',
  'bundle' => 'well_child_head_circumference',
  'description' => t('Exports the Well Child Head Circumference bundle.'),
  'class' => 'HedleyRestfulWellChildHeadCircumference',
  'authentication_types' => TRUE,
);
