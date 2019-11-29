<?php

/**
 * @file
 * Populate the type for all clinics.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_health_center/scripts/populate-encounter-type.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

$query = new EntityFieldQuery();
$query
  ->entityCondition('entity_type', 'node')
  ->propertyCondition('type', 'prenatal_participant')
  ->propertyCondition('status', NODE_PUBLISHED)
  ->propertyOrderBy('nid', 'ASC');

$result = $query->execute();

if (empty($result['node'])) {
  drush_print('No Prenatal participants found!');

  return;
}

$ids = array_keys($result['node']);

drush_print(format_string('Found @count participants!', ['@count' => count($ids)]));

$participants = node_load_multiple($ids);

foreach ($participants as $participant) {
  $wrapper = entity_metadata_wrapper('node', $participant);
  $clinic_name = $wrapper->label();

  drush_print('Assigning Antenatal type to participant.');

  $wrapper->field_encounter_type->set('antenatal');
  $wrapper->save();
}


drush_print('Done!');
